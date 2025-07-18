module Ri where

type Id = String

data Tipo
     = TDouble
     | TInt
     | TString
     | TVoid
     deriving (Show, Eq)

data TConst
     = CDouble Double
     | CInt Int
     deriving (Show, Eq)

data Expr
     = Add Expr Expr
     | Sub Expr Expr 
     | Mul Expr Expr 
     | Div Expr Expr 
     | Neg Expr 
     | Const TConst
     | IdVar String
     | Chamada Id [Expr]
     | Lit String
     | IntDouble Expr
     | DoubleInt Expr
     deriving (Show, Eq)

data ExprR 
     = Req Expr Expr
     | Rdif Expr Expr
     | Rle Expr Expr
     | Rge Expr Expr
     | Rlt Expr Expr
     | Rgt Expr Expr
     deriving Show

data ExprL
     = And ExprL ExprL
     | Or  ExprL ExprL
     | Not ExprL
     | Rel ExprR
     deriving Show

-- Var = Nome da variável :#: (Tipo da variável, Posição na memória (pilha))
data Var
     = Id :#: (Tipo, Int)
     deriving Show

data Funcao
     = Id :->: ([Var], Tipo)
     deriving Show

-- Prog [Todas as funcoes] [(Id da funcao, Parametros da funcao ++ Declaracoes da funcao, Bloco da funcao)] [Declaracoes do bloco principal] Bloco principal
data Programa 
     = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco
     deriving Show

type Bloco = [Comando]
data Comando
     = Ret (Maybe Expr)
     | If ExprL Bloco Bloco
     | While ExprL Bloco
     | Atrib Id Expr
     | Imp Expr
     | Leitura Id
     | Proc Id [Expr]
     deriving Show