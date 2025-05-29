module Token where

data Token
  = NUM Double
  | ADD
  | SUB
  | MUL
  | DIV
  | LPAR
  | RPAR
  | TEQ
  | TDIF
  deriving (Eq, Show)
  
