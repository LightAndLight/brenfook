module Interpreter where

import Control.Monad.Writer
import Data.Char
import Data.Functor
import Data.Monoid

import Instructions

data Tape a
  = Tape
  { left :: [a]
  , focus :: a
  , right :: [a]
  }

newTape :: a -> Tape a
newTape a = Tape { left = [], focus = a, right = repeat a }

runInsts :: Bool -> [Inst] -> Tape Int -> IO (Tape Int)
runInsts debug insts tape = go insts tape
  where
    runInst :: Inst -> Tape Int -> IO (Tape Int)
    runInst Inc (Tape l f r) = pure $ Tape l (f + 1) r
    runInst Dec (Tape l f r)
      | f == 0 = pure $ Tape l f r
      | otherwise = pure $ Tape l (f - 1) r
    runInst ShiftL (Tape [] f r) = pure $ Tape [] f r
    runInst ShiftL (Tape (next:l) f r) = pure $ Tape l next (f:r)
    runInst ShiftR (Tape l f (next:r)) = pure $ Tape (f:l) next r
    runInst Print tape = putStr [chr $ focus tape] $> tape
    runInst Input tape = do
      input <- getChar
      pure $ tape { focus = ord input }
    runInst (Loop insts) tape
      | focus tape == 0 = pure tape
      | otherwise = do
          tape' <- runInsts debug insts tape
          runInst (Loop insts) tape'
    runInst _ tape = pure tape

    go [] tape = pure tape
    go (inst:rest) tape
      | inst == Abort && debug = pure tape
      | otherwise = do
          tape' <- runInst inst tape
          runInsts debug rest tape'

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
loop_after_clear (Loop [Dec] : Loop _ : rest) = do
  tell $ Any True
  pure $ Loop [Dec] : rest
loop_after_clear insts = pure insts

inc_before_clear :: Rule
inc_before_clear (Inc : Loop [Dec] : rest) = do
  tell $ Any True
  pure $ Loop [Dec] : rest
inc_before_clear (Dec : Loop [Dec] : rest) = do
  tell $ Any True
  pure $ Loop [Dec] : rest
inc_before_clear insts = pure insts

shift_elim :: Rule
shift_elim (ShiftR : ShiftL : rest) = do
  tell $ Any True
  pure rest
shift_elim (ShiftL : ShiftR : rest) = do
  tell $ Any True
  pure rest
shift_elim insts = pure insts

inc_elim :: Rule
inc_elim (Inc : Dec : rest) = do
  tell $ Any True
  pure rest
inc_elim (Dec : Inc : rest) = do
  tell $ Any True
  pure rest
inc_elim insts = pure insts

rules = [loop_after_clear, shift_elim, inc_elim, inc_before_clear]

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
