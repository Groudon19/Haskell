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
  '>' {TGT}
  '&&'{TAND}
  '||'{TOR}
  '!' {TNOT}
  NumDouble {NUMDOUBLE $$}
  NumInt {NUMINT $$}


%%

Inicio: ExprL               {Left $1}
      | Expr                {Right $1}

ExprL : ExprL '&&' Bool     {And $1 $3}
      | ExprL '||' Bool     {Or $1 $3}
      | Bool                {$1}

Bool  : ExprR               {Rel $1}
      | '(' ExprL ')'       {$2}
      | '!' Bool            {Not $2}

ExprR : Expr '==' Expr      {Req $1 $3}
      | Expr '/=' Expr      {Rdif $1 $3}
      | Expr '<=' Expr      {Rle $1 $3}
      | Expr '>=' Expr      {Rge $1 $3}
      | Expr '<'  Expr      {Rlt $1 $3}
      | Expr '>'  Expr      {Rgt $1 $3}

Expr  : Expr '+' Term       {Add $1 $3}
      | Expr '-' Term       {Sub $1 $3}
      | Term                {$1}

Term  : Term '*' Factor     {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

Factor: TConst                 {Const $1}
      | '(' Expr ')'        {$2}  
      | '-' Factor          {Neg $2}

TConst: NumDouble {CDouble $1}
      | NumInt    {CInt $1}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do putStr "Expressão:"
          s <- getLine
          case (calc (L.alexScanTokens s)) of
            Left l  -> print l
            Right r -> print r
}