module Ri where

type Id = String

data Inicio
     = Expr Expr
     | ExprL ExprL
     | Declaracoes [Var]
     deriving Show

data Tipo
     = TDouble
     | TInt
     | TString
     | TVoid
     deriving (Show, Eq)

data TConst
     = CDouble Double
     | CInt Int
     deriving Show

data Expr
     = Add Expr Expr
     | Sub Expr Expr 
     | Mul Expr Expr 
     | Div Expr Expr 
     | Neg Expr 
     | Const TConst
     | IdVar String
     | Lit String
     deriving Show

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

data Var
     = Id :#: (Tipo, Int)
     deriving Show