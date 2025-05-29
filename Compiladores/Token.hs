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
  deriving (Eq, Show)
  
