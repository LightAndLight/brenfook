module Main where

import Control.Monad
import System.Environment
import System.Exit
import Text.Megaparsec

import Interpreter
import Parser

main :: IO ()
main = do
  (fileName:rest) <- getArgs
  let size = case rest of
        [] -> 30000
        n:_ -> read n
  insts <- runParser instsP fileName <$> readFile fileName
  case insts of
    Left err -> die $ parseErrorPretty err
    Right insts -> void . runInsts' size False $ optimize insts
