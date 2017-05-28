module Algorithms where

import Control.Monad
import Data.Traversable

import BF

asAscii :: Address -> BF ()
asAscii val = do
  ten <- new 10
  num_digits <- new 0
  digits <- array 10

  remaining <- alloc
  mov val remaining

  doWhile remaining $ do
    incr num_digits
    (q, r) <- dvd remaining ten
    mov q remaining
    free q

    res <- sub ten num_digits
    writeIx r res digits

  while num_digits $ do
    res <- sub ten num_digits
    val <- readIx res digits
    replicateM_ 48 $ incr val
    wrt val
    decr num_digits

  free remaining
  freeArray digits
  free num_digits
  free ten
      
