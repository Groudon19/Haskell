{
module ParserRi where

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
  '{' {LBRC}
  '}' {RBRC}
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
  '=' {TATRIB}
  'int' {INT}
  'double' {DOUBLE}
  'string' {STRING}
  'void' {VOID}
  'return' {RETURN}
  'if' {TIF}
  'else' {TELSE}
  'while' {TWHILE}
  'print'  {TPRINT}
  'read'   {TREAD}
  NumDouble {NUMDOUBLE $$}
  NumInt {NUMINT $$}
  Id {ID $$}
  Literal {LIT $$}


%%

-- TODO: Lidar com blocos vazios
-- TODO: Declarações após comandos
-- TODO: for (ele vai pedir ctz)

-- Inicio
Inicio: Expr                {Expr $1}
      | ExprL               {ExprL $1}
      | BlocoPrincipal      {BlocoPrincipal $1}
      | Funcao              {Funcao $1}

-- (Funcao, ([Var], [Comando]))
Funcao: TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal {($2 :->: ($4, $1), $6)}
      | TipoRetorno Id '(' ')' BlocoPrincipal                {($2 :->: ([], $1), $5)}

-- ([Var], [Comando])
BlocoPrincipal: '{' Declaracoes ListaCmd '}' {($2, $3)}
              | Bloco                        {([], $1)}          

--Tipo
TipoRetorno: Tipo              {$1}
           | 'void'            {TVoid}

-- [Comando]
Bloco: '{' ListaCmd '}'  {$2}

-- [Comando]
ListaCmd: ListaCmd Comando {$1 ++ [$2]}
        | Comando          {[$1]}

--[Var]
DeclParametros: DeclParametros ',' Parametro {$1 ++ [$3]}
              | Parametro                    {[$1]}

-- Var
Parametro: Tipo Id          {$2 :#: ($1, 0)}

-- [Var]
Declaracoes: Declaracoes Declaracao {$1 ++ $2}
           | Declaracao       {$1}

-- Comando
Comando: CmdSe                {$1}
       | CmdEnquanto          {$1}
       | CmdAtrib             {$1}
       | CmdEscrita           {$1}
       | CmdLeitura           {$1}
       | ChamadaProc          {$1}  
       | Retorno              {$1}

-- Comando
Retorno: 'return' Expr ';'    {Ret (Just $2)}
       | 'return' ';'         {Ret Nothing}

-- Comando
CmdSe: 'if' '(' ExprL ')' Bloco 'else' Bloco {If $3 $5 $7}
     | 'if' '(' ExprL ')' Bloco              {If $3 $5 []}

-- Comando
CmdEnquanto: 'while' '(' ExprL ')' Bloco {While $3 $5}

-- Comando
CmdAtrib: Id '=' Expr ';'      {Atrib $1 $3}

-- Comando
CmdEscrita: 'print' '(' Expr ')' ';'  {Imp $3}

-- Comando
CmdLeitura: 'read' '(' Id ')' ';'  {Leitura $3}

-- Comando
ChamadaProc: ChamadaFuncao ';' {$1}

-- Comando
ChamadaFuncao: Id '(' ListaParametros ')' {Proc $1 $3}
             | Id '(' ')'                 {Proc $1 []}

-- [Expr]
ListaParametros: ListaParametros ',' Expr {$1 ++ [$3]}
               | Expr                     {[$1]}

-- [Var]
Declaracao: Tipo ListaId ';' {map (\id -> id:#:($1, 0)) $2}

-- Tipo
Tipo  : 'double' {TDouble}
      | 'int'    {TInt}
      | 'string' {TString}

-- [String]
ListaId: ListaId ',' Id      {$1 ++ [$3]}
       | Id                  {[$1]}     

-- ExprL
ExprL : ExprL '&&' Bool     {And $1 $3}
      | ExprL '||' Bool     {Or $1 $3}
      | Bool                {$1}

-- ExprL
Bool  : ExprR               {Rel $1}
      | '(' ExprL ')'       {$2}
      | '!' Bool            {Not $2}

-- ExprR
ExprR : Expr '==' Expr      {Req $1 $3}
      | Expr '/=' Expr      {Rdif $1 $3}
      | Expr '<=' Expr      {Rle $1 $3}
      | Expr '>=' Expr      {Rge $1 $3}
      | Expr '<'  Expr      {Rlt $1 $3}
      | Expr '>'  Expr      {Rgt $1 $3}

-- Expr
Expr  : Expr '+' Term       {Add $1 $3}
      | Expr '-' Term       {Sub $1 $3}
      | Term                {$1}

-- Expr
Term  : Term '*' Factor     {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

-- Expr
Factor: TConst                      {Const $1}
      | '(' Expr ')'                {$2}  
      | '-' Factor                  {Neg $2}
      | Id                          {IdVar $1}
      | Id '(' ListaParametros ')'  {Chamada $1 $3}
      | Id '(' ')'                  {Chamada $1 []}
      | Literal                     {Lit $1}

-- TConst
TConst: NumDouble {CDouble $1}
      | NumInt    {CInt $1}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do s <- readFile "teste.txt"
          print (calc (L.alexScanTokens s))

manual = do putStr "Expressão:"
            s <- getLine
            print (calc (L.alexScanTokens s))
      --     case (calc (L.alexScanTokens s)) of
      --       Expr r  -> print r
      --       ExprL l -> print l
      --       Declaracoes d -> print d
}