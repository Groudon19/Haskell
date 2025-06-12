{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]
$letter = [a-zA-Z]
@numDouble = $digit+(\.$digit+)?
@numInt = $digit+
@id = ($letter | \_)($letter | $digit | \_)*
@literal = \"([^\"]|\\.)*\" --"(todo caracter que nao é aspas duplas | escape sequence)*"

tokens :-

<0> $white+ ;

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
<0> "," {\s -> COMMA}
<0> ";" {\s -> TEND}
<0> "=" {\s -> TATRIB}

<0> "int" {\s -> INT}
<0> "double" {\s -> DOUBLE}
<0> "string" {\s -> STRING}
<0> "void" {\s -> VOID}

<0> "return" {\s -> RETURN}
<0> "print" {\s -> TPRINT}
<0> "read" {\s -> TREAD}

<0> @numDouble{\s -> NUMDOUBLE (read s)}
<0> @numInt{\s -> NUMINT (read s)}
<0> @id{\s -> ID s}
<0> @literal{\s -> LIT (init (tail s))}


{
-- As acoes tem tipo :: String -> Token

testLex = do s <- getLine
             print (alexScanTokens s)
}