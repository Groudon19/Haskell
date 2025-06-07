{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]          -- digits
@num = $digit+(\.$digit+)?

tokens :-

<0> $white+ ;
<0> @num {\s -> NUM (read s)}
<0> "=="{\s -> TEQ}
<0> "/="{\s -> TDIF}
<0> "<="{\s -> TLE}
<0> ">="{\s -> TGE}
<0> "<" {\s -> TLT}
<0> ">" {\s -> TGT}
<0> "&&"{\s -> TAND}
<0> "||"{\s -> TOR}
<0> "!" {\s -> TNOT}
<0> "+" {\s -> ADD}  
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}  
<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}

{
-- As acoes tem tipo :: String -> Token

testLex = do s <- getLine
             print (alexScanTokens s)
}