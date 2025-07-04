{
module Parser where

import Token
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
  Num {NUM $$}


%%

Inicio: ExprL               {Left $1}
      | Expr                {Right $1}

ExprL : ExprL '&&' Bool     {$1 && $3}
      | ExprL '||' Bool     {$1 || $3}
      | Bool                {$1}

Bool  : ExprR               {$1}
      | '(' ExprL ')'       {$2}
      | '!' Bool            {not $2}

ExprR : Expr '==' Expr      {$1 == $3}
      | Expr '/=' Expr      {$1 /= $3}
      | Expr '<=' Expr      {$1 <= $3}
      | Expr '>=' Expr      {$1 >= $3}
      | Expr '<'  Expr      {$1 < $3}
      | Expr '>'  Expr      {$1 > $3}

Expr  : Expr '+' Term       {$1 + $3}
      | Expr '-' Term       {$1 - $3}
      | Term                {$1}

Term  : Term '*' Factor     {$1 * $3}
      | Term '/' Factor     {$1 / $3}
      | Factor              {$1}

Factor: Num                 {$1}
      | '(' Expr ')'        {$2}
      | '-' Factor          {negate $2}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do putStr "Expressão:"
          s <- getLine
          case (calc (L.alexScanTokens s)) of
            Left l  -> print l
            Right r -> print r
}