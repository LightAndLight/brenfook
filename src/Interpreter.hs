{-# language FlexibleContexts #-}
{-# language RankNTypes #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Monoid
import qualified Data.Vector.Mutable as M
import Data.Vector.Mutable (IOVector)
import Data.Vector (Vector, freeze)

import Instructions

--
--
--
-- Faster mutable vector version
--
--
--
runInsts' :: Int -> Bool -> [Inst] -> IO (Vector Int)
runInsts' size debug insts = do
  tape <- M.new size
  M.set tape 0
  void $
    flip runReaderT tape .
    flip runStateT 0 $
    traverse_ runInst insts
  freeze tape
  where
    onCurrentCell
      :: (MonadIO m, MonadState Int m, MonadReader (IOVector a) m)
      => (IOVector a -> Int -> m b) -> m b
    onCurrentCell f = do
      tape <- ask
      pointer <- get
      f tape pointer

    readCurrentCell
      :: (MonadIO m, MonadState Int m, MonadReader (IOVector a) m)
      => m a
    readCurrentCell = onCurrentCell (\t p -> liftIO $ M.read t p)

    writeCurrentCell
      :: (MonadIO m, MonadState Int m, MonadReader (IOVector a) m)
      => a -> m ()
    writeCurrentCell val = onCurrentCell (\t p -> liftIO $ M.write t p val)

    runInst inst =
      case inst of
        Inc n 
          | n < 0 -> runInst $ Dec (-n)
          | otherwise ->
              onCurrentCell (\t p -> liftIO $ M.modify t (+n) p)
        Dec n
          | n < 0 -> runInst $ Inc (-n)
          | otherwise ->
              onCurrentCell (\t p -> liftIO $ M.modify t (\v -> if v - n >= 0 then v - n else 0) p)

        ShiftR n
          | n < 0 -> runInst $ ShiftL (-n)
          | otherwise ->
              modify $ \p ->
                if p + n >= size
                  then size - 1
                  else p + n

        ShiftL n
          | n < 0 -> runInst $ ShiftR (-n)
          | otherwise ->
              modify $ \p ->
                if p - n <= 0
                  then 0
                  else p - n
        
        Print -> do
          val <- readCurrentCell
          liftIO $ putStr [chr val]

        Input -> do
          input <- liftIO getChar
          writeCurrentCell $ ord input

        Loop inner -> do
          val <- readCurrentCell
          when (val /= 0) $ do
            traverse_ runInst inner
            runInst $ Loop inner

        Clear -> writeCurrentCell 0

-- 
--
--
-- Slower zipper-based version
-- 
--
--
data Tape a
  = Tape
  { left :: [a]
  , focus :: a
  , right :: [a]
  }

newTape :: a -> Tape a
newTape a = Tape { left = [], focus = a, right = repeat a }


runInsts :: Bool -> [Inst] -> Tape Int -> IO (Tape Int)
runInsts debug = go
  where
    runInst :: Inst -> Tape Int -> IO (Tape Int)
    runInst (Inc n) (Tape l f r)
      | n < 0 = runInst (Dec (-n)) (Tape l f r)
      | otherwise = pure $ Tape l (f + n) r
    runInst (Dec n) (Tape l f r)
      | f - n <= 0 = pure $ Tape l 0 r
      | otherwise = pure $ Tape l (f - n) r
    runInst (ShiftL n) (Tape l f r) 
      | n == 0 = pure $ Tape l f r
      | n < 0 = runInst (ShiftR (-n)) (Tape l f r)
      | [] <- l = pure $ Tape [] f r
      | otherwise =
          case drop (n-1) l of
            [] ->
              let (f':l') = reverse l
              in pure $ Tape [] f' (l' ++ f : r)
            f':l' -> pure $ Tape l' f' (reverse (take (n-1) l) ++ f : r)
    runInst (ShiftR n) (Tape l f r) 
      | n == 0 = pure $ Tape l f r
      | n < 0 = runInst (ShiftL (-n)) (Tape l f r)
      | otherwise =
          let f':r' = drop (n-1) r
          in pure $ Tape (reverse (take (n-1) r) ++ f : l) f' r' 
    runInst Print tape = putStr [chr $ focus tape] $> tape
    runInst Input tape = do
      input <- getChar
      pure $ tape { focus = ord input }
    runInst (Loop insts) tape
      | focus tape == 0 = pure tape
      | otherwise = do
          tape' <- runInsts debug insts tape
          runInst (Loop insts) tape'
    runInst Clear (Tape l f r) = pure $ Tape l 0 r
    runInst _ tape = pure tape

    go [] tape = pure tape
    go (inst:rest) tape
      | inst == Abort && debug = pure tape
      | otherwise = do
          tape' <- runInst inst tape
          runInsts debug rest tape'

--
--
--
-- Optimization
--
--
--

type Rule = [Inst] -> Writer Any [Inst]

bottomUpOnInsts :: Monad m => ([Inst] -> m [Inst]) -> [Inst] -> m [Inst]
bottomUpOnInsts f [] = f []
bottomUpOnInsts f (x:xs) = do
  res <- bottomUpOnInsts f xs
  case x of
    Loop insts -> do
      x' <- Loop <$> bottomUpOnInsts f insts
      f (x' : res)
    _ -> f (x : res)

loop_after_clear :: Rule
loop_after_clear (Loop [Dec 1] : Loop _ : rest) = do
  tell $ Any True
  pure $ Loop [Dec 1] : rest
loop_after_clear (Clear : Loop _ : rest) = do
  tell $ Any True
  pure $ Clear : rest
loop_after_clear insts = pure insts

inc_before_clear :: Rule
inc_before_clear (Inc _ : Loop [Dec 1] : rest) = do
  tell $ Any True
  pure $ Loop [Dec 1] : rest
inc_before_clear (Dec _ : Loop [Dec 1] : rest) = do
  tell $ Any True
  pure $ Loop [Dec 1] : rest
inc_before_clear insts = pure insts

shift_elim :: Rule
shift_elim (ShiftR n : ShiftL m : rest) = do
  tell $ Any True
  pure $ ShiftR (n - m) : rest
shift_elim (ShiftL n : ShiftR m : rest) = do
  tell $ Any True
  pure $ ShiftL (n - m) : rest
shift_elim insts = pure insts

inc_elim :: Rule
inc_elim (Inc n : Dec m : rest) = do
  tell $ Any True
  pure $ Inc (n - m) : rest
inc_elim (Dec n : Inc m : rest) = do
  tell $ Any True
  pure $ Dec (n - m) : rest
inc_elim insts = pure insts

changes_before_input :: Rule
changes_before_input (Inc _ : Input : rest) = do
  tell $ Any True
  pure $ Input : rest
changes_before_input (Dec _ : Input : rest) = do
  tell $ Any True
  pure $ Input : rest
changes_before_input (Input : Input : rest) = do
  tell $ Any True
  pure $ Input : rest
changes_before_input insts = pure insts

inc_dec_accum :: Rule
inc_dec_accum (Inc n : Inc m : rest) = do
  tell $ Any True
  pure $ Inc (n + m) : rest
inc_dec_accum (Dec n : Dec m : rest) = do
  tell $ Any True
  pure $ Dec (n + m) : rest
inc_dec_accum insts = pure insts

no_op_elim :: Rule
no_op_elim (Inc 0 : rest) = do
  tell $ Any True
  pure rest
no_op_elim (Dec 0 : rest) = do
  tell $ Any True
  pure rest
no_op_elim (ShiftR 0 : rest) = do
  tell $ Any True
  pure rest
no_op_elim (ShiftL 0 : rest) = do
  tell $ Any True
  pure rest
no_op_elim insts = pure insts

rewrite_clears :: Rule
rewrite_clears (Loop [Dec 1] : rest) = do
  tell $ Any True
  pure $ Clear : rest
rewrite_clears insts = pure insts

rules =
  [ loop_after_clear
  , shift_elim
  , inc_elim
  , inc_before_clear
  , changes_before_input
  , inc_dec_accum
  , no_op_elim
  , rewrite_clears
  ]

optimize :: [Inst] -> [Inst]
optimize [] = []
optimize insts =
  let (insts', Any b) = runWriter (go rules insts)
  in if b
    then optimize insts'
    else insts'
  where
    go [] insts = pure insts
    go (rule:rest) insts = do
      insts' <- bottomUpOnInsts rule insts
      go rest insts'
