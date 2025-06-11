module Token where

data Token
  = NUMINT Int
  | NUMDOUBLE Double
  | LIT String
  | ID String
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
  | COMMA
  deriving (Eq, Show)
  
