module Token where

data Token
  = NUMINT Int
  | NUMDOUBLE Double
  | LIT String
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
  | TOR
  | TNOT
  deriving (Eq, Show)
  
