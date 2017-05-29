module Parser (instsP) where

import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.String

import Instructions

inc :: Parser Inst
inc = char '+' $> Inc 1

dec :: Parser Inst
dec = char '-' $> Dec 1

shiftR :: Parser Inst
shiftR = char '>' $> ShiftR 1

shiftL :: Parser Inst
shiftL = char '<' $> ShiftL 1

write :: Parser Inst
write = char '.' $> Print

input :: Parser Inst
input = char ',' $> Input

loop :: Parser Inst
loop = char '[' *> fmap Loop (someTill inst $ char ']')

inst :: Parser Inst
inst = (
  inc <|>
  dec <|>
  shiftR <|>
  shiftL <|>
  write <|>
  input <|>
  loop
  ) <* skipMany spaceChar

instsP :: Parser [Inst]
instsP = some inst <* eof
