{-# language GeneralizedNewtypeDeriving #-}

module BF
  ( BF
  , genInsts
  , Address
  , dvd
  , notz
  , isz
  , gt
  , gte
  , lt
  , lte
  , alloc
  , free
  , incr
  , decr
  , wrt
  , rd
  , clear
  , load
  , (.=)
  , while
  , doWhile
  , ifThen
  , mov
  , movm
  , abort
  , neg
  , sub
  , new
  , array
  , freeArray
  , readIx
  , writeIx
  , add
  , up
  , down
  ) where

import Debug.Trace

import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.Foldable
import Data.Maybe

import Instructions

newtype BF a
  = BF
  { runBF :: WriterT [Inst] (State StackState) a
  } deriving (Functor, Applicative, Monad)

data StackState = StackState { pointer :: Int, addresses :: [Int] }
  deriving (Eq, Show)

newStack = StackState { pointer = 0, addresses = [0..] }

newtype Address = Address { getAddress :: Int }

up :: Address -> Address
up (Address a) = Address (a + 1)

down :: Address -> Address
down (Address a) = Address (a - 1)

alloc :: BF Address
alloc = BF $ do
  (addr:rest) <- lift $ gets addresses
  lift . modify $ \s -> s { addresses = rest }
  pure $ Address addr

free :: Address -> BF ()
free (Address addr) = BF .
  lift . modify $ \s -> s { addresses = addr : addresses s }

goto :: Address -> BF ()
goto (Address addr) =
  BF $ do
    sp <- lift $ gets pointer
    lift $ modify (\s -> s { pointer = addr })
    tell [ShiftR $ addr - sp]

incs :: Int -> BF ()
incs n = BF . tell $ [Inc n]

incr :: Address -> BF ()
incr addr = do
  goto addr
  incs 1

clear :: Address -> BF ()
clear addr = do
  goto addr
  BF . tell $ [Loop [Dec 1]]

wrt :: Address -> BF ()
wrt addr = do
  goto addr
  BF $ tell [Print]

rd :: Address -> BF ()
rd addr = do
  goto addr
  BF $ tell [Input]

load :: Int -> Address -> BF ()
load val addr = do
  goto addr 
  clear addr
  incs val

(.=) :: Address -> Int -> BF ()
(.=) = flip load

new :: Int -> BF Address
new val = do
  addr <- alloc
  addr .= val
  pure addr

while :: Address -> BF () -> BF ()
while addr code = do
  goto addr
  st <- BF $ lift get
  let insts =
        snd .
        flip evalState st .
        runWriterT .
        runBF $ do
          code
          goto addr
  BF $ tell [Loop insts]

doWhile :: Address -> BF () -> BF ()
doWhile addr code = do
  code
  while addr code

ifThen :: Address -> BF () -> BF ()
ifThen addr code = do
  temp <- alloc
  mov addr temp
  while temp $ do
    code
    temp .= 0

mov :: Address -> Address -> BF ()
mov from to = movm from [to]

movm :: Address -> [Address] -> BF ()
movm from tos = do
  traverse_ clear tos
  temp <- alloc
  while from $ do
    decr from
    incr temp
    traverse_ incr tos
  while temp $ do
    decr temp
    incr from
  free temp

abort :: BF ()
abort = BF $ tell [Abort]

decs :: Int -> BF ()
decs n = BF . tell $ [Dec n]

decr :: Address -> BF ()
decr addr = do
  goto addr
  decs 1

neg :: Address -> BF ()
neg addr = do
  flag <- new 1
  ifThen addr $ do
    decr flag
    clear addr
  ifThen flag $ incr addr
  free flag

-- | Subtract the value of the second address from the value of the first
-- Returns a pointer to the result
sub :: Address -> Address -> BF Address
sub left right = do
  result <- alloc
  temp <- alloc
  mov right temp
  mov left result
  while temp $ do
    decr temp
    decr result
  free temp
  pure result

add :: Address -> Address -> BF Address
add left right = do
  result <- new 0
  mov right result

  temp <- alloc
  mov left temp

  while temp $ do
    decr temp
    incr result

  free temp
  pure result

isz :: Address -> BF Address
isz addr = do
  res <- new 1
  ifThen addr $ decr res
  pure res

notz :: Address -> BF Address
notz addr = do
  res <- new 0
  ifThen addr $ incr res
  pure res

-- | Check if the value of the first address is GT the value of the second
-- Returns an address whose value is 1 if true or 0 if false
gt :: Address -> Address -> BF Address
gt left right = do
  left_minus_right <- sub left right
  res <- notz left_minus_right
  free left_minus_right
  pure res

gte :: Address -> Address -> BF Address
gte left right = do
  incr left
  res <- gt left right
  decr left
  pure res

lte :: Address -> Address -> BF Address
lte left right = gte right left

lt :: Address -> Address -> BF Address
lt left right = do
  decr right
  res <- lte left right
  incr right
  pure res

-- | Divides first value by second
--
-- Returns two pointers, first to the quotient, second to remainder
-- Currently dividing by zero results in an infinite loop
--
dvd :: Address -> Address -> BF (Address, Address)
dvd top bottom = do
  quotient <- new 0

  remainder <- alloc
  mov top remainder

  should_loop <- alloc
  t_gte_b <- gte top bottom
  mov t_gte_b should_loop
  free t_gte_b

  while should_loop $ do
    incr quotient
    res <- sub remainder bottom
    mov res remainder
    free res

    should_loop' <- gte remainder bottom
    mov should_loop' should_loop
    free should_loop'

  pure (quotient, remainder)

data Array = Array { arraySize :: Int, arrayAddress :: Address }

takeRunOf :: (Num a, Eq a) => Int -> [a] -> ([a], [a])
takeRunOf 0 xs = ([], xs)
takeRunOf n [] = ([], [])
takeRunOf 1 (x:rest) = ([x], rest)
takeRunOf n (x:y:rest)
  | y == x + 1
  , (l, rest') <- takeRunOf (n-1) (y:rest)
  = (x:l, rest')
  | otherwise = ([x], y:rest)

splitRunsOf :: (Num a, Eq a) => Int -> [a] -> [[a]]
splitRunsOf n [] = []
splitRunsOf n xs =
  let (run, rest) = takeRunOf n xs
  in run : splitRunsOf n rest

takeFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
takeFirst pred = go []
  where
    go seen [] = Nothing
    go seen (x:xs)
      | pred x = Just (x, reverse seen ++ xs)
      | otherwise = go (x:seen) xs

takeFirstRunOf :: (Num a, Eq a) => Int -> [a] -> Maybe ([a], [a])
takeFirstRunOf n xs = do
  (run, rest) <- takeFirst ((==) n . length) $ splitRunsOf n xs
  pure (run, concat rest)

array :: Int -> BF Array
array size = do
  addrs <- BF $ lift $ gets addresses
  let Just (addr:_, addrs') = takeFirstRunOf (size+4) addrs
  BF . lift . modify $ \s -> s { addresses = addrs' }
  pure . Array size $ Address addr

freeArray :: Array -> BF ()
freeArray (Array size (Address addr)) =
  traverse_ (free . Address) [addr+size+4-1, addr+size+4-2 .. addr]

-- | Writes the values pointed to by the first argument to the index
-- pointed to by the second
--
-- Credit: http://www.inshame.com/2008/02/efficient-brainfuck-tables.html
-- >[
--   >>>[
--     -<<<<
--     +>>>>
--      ]
--   <[->+<]
--   <[->+<]
--   <[->+<]
--   >-
--  ]
-- >>>[-]
-- <[->+<]
-- <[
--   [-<+>]
--   <<<
--   [
--     ->>>>
--     +<<<<
--   ]
--   >>-
--  ]<<
--
writeIx :: Address -> Address -> Array -> BF ()
writeIx val ix a = do
  let addr = arrayAddress a

  movm ix [up addr, up . up $ addr]

  mov val . up . up $ up addr
  goto $ arrayAddress a

  BF $ tell
    [ ShiftR 1
    , Loop
      [ ShiftR 3, Loop
        [ Dec 1, ShiftL 4
        , Inc 1, ShiftR 4
        ]
      , ShiftL 1, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
      , ShiftL 1, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
      , ShiftL 1, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
      , ShiftR 1, Dec 1
      ]
    , ShiftR 3, Loop [Dec 1]
    , ShiftL 1, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
    , ShiftL 1, Loop
      [ Loop [ Dec 1, ShiftL 1, Inc 1, ShiftR 1 ]
      , ShiftL 3
      , Loop
        [ ShiftL 3, Loop
          [ Dec 1, ShiftR 4
          , Inc 1, ShiftL 4
          ]
        ]
      , ShiftR 2, Dec 1 ]
    , ShiftL 2
    ]

-- | Returns a pointer to a copy of the value at the index
-- pointed to by the first argument 
--
-- Credit: http://www.inshame.com/2008/02/efficient-brainfuck-tables.html
-- >[
--   >>>[
--     -<<<<+>>>>
--   ]
--   <<[->+<]
--   <[->+<]
--   >-
-- ]
-- >>>[
--   -<+<<+>>>
-- ]
-- <<<[->>>+<<<]
-- >[
--   [-<+>]
--   >[-<+>]
--   <<<<[->>>>+<<<<]
--   >>-
-- ]
-- <<
--
readIx :: Address -> Array -> BF Address
readIx ix a = do
  let addr = arrayAddress a

  movm ix [up addr, up . up $ addr]

  goto addr

  BF $ tell
    [ ShiftR 1, Loop
      [ ShiftR 3, Loop
        [ Dec 1, ShiftL 4
        , Inc 1, ShiftR 4
        ]
      , ShiftL 2, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
      , ShiftL 1, Loop [ Dec 1, ShiftR 1, Inc 1, ShiftL 1 ]
      , ShiftR 1, Dec 1
      ]
    , ShiftR 3, Loop
      [ Dec 1, ShiftL 1, Inc 1, ShiftL 2, Inc 1, ShiftR 3 ]
    , ShiftL 3, Loop
      [ Dec 1, ShiftR 3, Inc 1, ShiftL 3 ]
    , ShiftR 1, Loop
      [ Loop [ Dec 1, ShiftL 1, Inc 1, ShiftR 1 ]
      , ShiftR 1, Loop [ Dec 1, ShiftL 1, Inc 1, ShiftR 1 ]
      , ShiftL 4, Loop
        [ Dec 1, ShiftR 4
        , Inc 1, ShiftL 4
        ]
      , ShiftR 2, Dec 1
      ]
    , ShiftL 2
    ]

  goto addr
  res <- alloc
  mov (up . up . up $ addr) res
  clear (up . up . up $ addr)
  pure res

genInsts :: BF a -> [Inst]
genInsts = snd . flip evalState newStack . runWriterT . runBF
