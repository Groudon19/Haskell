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
  | TGT
  | TAND
  deriving (Eq, Show)
  
