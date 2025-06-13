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

  | TIF
  | TELSE
  | TPRINT
  | TREAD
  | RETURN
  
  | ADD
  | SUB
  | MUL
  | DIV
  | LPAR
  | RPAR
  | LBRC
  | RBRC
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
  
