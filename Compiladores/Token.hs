module Token where

data Token
  = NUMINT Int
  | NUMDOUBLE Double
  | LIT String
  | ID String

  | DOUBLE
  | INT
  | STRING
  | VOID

  | TPRINT
  | TREAD
  | RETURN
  
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
  | TEND --;
  | TATRIB
  deriving (Eq, Show)
  
