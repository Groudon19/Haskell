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
  | TLE
  | TGE
  | TLT
  deriving (Eq, Show)
  
