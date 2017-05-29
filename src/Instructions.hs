module Instructions where

data Inst
  = Inc Int
  | Dec Int
  | ShiftL Int
  | ShiftR Int
  | Print
  | Input
  | Loop [Inst]
  | Abort
  | Clear
  deriving (Eq, Show)

printInst :: Inst -> String
printInst (Inc n) = replicate n '+'
printInst (Dec n) = replicate n '-'
printInst (ShiftL n)
  | n < 0 = printInst $ ShiftR (-n)
  | otherwise = replicate n '<'
printInst (ShiftR n)
  | n < 0 =  printInst $ ShiftL (-n)
  | otherwise = replicate n '>'
printInst Print = "."
printInst Input = ","
printInst (Loop insts) = "[" ++ (insts >>= printInst) ++ "]"
printInst Clear = "[-]"
printInst _ = ""
