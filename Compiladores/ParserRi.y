{
module Parser where

import Token
import Ri
import qualified Lex as L

}


%name calc
%tokentype { Token }
%error { parseError }
%token 
  '+' {ADD}
  '-' {SUB}
  '*' {MUL}
  '/' {DIV}
  '(' {LPAR}
  ')' {RPAR}
  '=='{TEQ}
  '/='{TDIF}
  '<='{TLE}
  '>='{TGE}
  '<' {TLT}
  Num {NUM $$}


%%

Inicio : ExprR              {Left $1}
       | Expr               {Right $1}

ExprR : Expr '==' Expr      {Req $1 $3}
      | Expr '/=' Expr      {Rdif $1 $3}
      | Expr '<=' Expr      {Rle $1 $3}
      | Expr '>=' Expr      {Rge $1 $3}
      | Expr '<'  Expr      {Rlt $1 $3}

Expr  : Expr '+' Term       {Add $1 $3}
      | Expr '-' Term       {Sub $1 $3}
      | Term                {$1}

Term  : Term '*' Factor     {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

Factor : Num                {Const $1}
       | '(' Expr ')'       {$2}  
       | '-' Factor         {Neg $2}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do putStr "Expressão:"
          s <- getLine
          print (calc (L.alexScanTokens s))
}