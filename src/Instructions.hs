module Instructions where

data Inst
  = Inc
  | Dec
  | ShiftL
  | ShiftR
  | Print
  | Input
  | Loop [Inst]
  | Abort
  deriving (Eq, Show)

printInst :: Inst -> String
printInst Inc = "+"
printInst Dec = "-"
printInst ShiftL = "<"
printInst ShiftR = ">"
printInst Print = "."
printInst Input = ","
printInst (Loop insts) = "[" ++ (insts >>= printInst) ++ "]"
printInst _ = ""
