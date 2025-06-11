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
  ',' {COMMA}
  ';' {TEND}
  'int' {INT}
  'double' {DOUBLE}
  'string' {STRING}
  NumDouble {NUMDOUBLE $$}
  NumInt {NUMINT $$}
  Id {ID $$}
  Literal {LIT $$}


%%

Inicio: Expr                {Expr $1}
      | ExprL               {ExprL $1}
      | Declaracoes         {Declaracoes $1}

Declaracoes: Declaracoes Declaracao {$1 ++ $2}
           | Declaracao       {$1}

Declaracao: Tipo ListaId ';' {map (\x -> x:#:($1, 0)) $2}

Tipo  : 'double' {TDouble}
      | 'int'    {TInt}
      | 'string' {TString}

ListaId: ListaId ',' Id      {$1 ++ [$3]}
       | Id                  {[$1]}     

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

Factor: TConst              {Const $1}
      | '(' Expr ')'        {$2}  
      | '-' Factor          {Neg $2}
      | Id                  {IdVar $1}
      | Literal             {Lit $1}

TConst: NumDouble {CDouble $1}
      | NumInt    {CInt $1}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do putStr "Expressão:"
          s <- getLine
          print (calc (L.alexScanTokens s))
      --     case (calc (L.alexScanTokens s)) of
      --       Expr r  -> print r
      --       ExprL l -> print l
}