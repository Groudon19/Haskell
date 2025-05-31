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
<0> "+" {\s -> ADD}  
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}  
<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}
<0> "=="{\s -> TEQ}
<0> "/="{\s -> TDIF}
<0> "<="{\s -> TLE}
<0> ">="{\s -> TGE}
<0> "<" {\s -> TLT}
-- colocar <= antes de < (pois se não consumiria o menor sempre e nunca chegaria no <=)

{
-- As acoes tem tipo :: String -> Token

testLex = do s <- getLine
             print (alexScanTokens s)
}