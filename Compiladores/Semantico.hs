module Semantico where

import System.IO
import Ri
import qualified Lex as L

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
  fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
  pure a = Result (False, "", a)
  Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

instance Monad Result where 
--  return a = Result (False, "", a)
  Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')
  
errorMsg s = Result (True, "Erro:"++s++"\n", ())

warningMsg s = Result (False, "Advertencia:"++s++"\n", ())

coercao op e1 e2 t1 t2 | t1 == t2                    = return (t1, op e1 e2)
                       | t1 == TInt && t2 == TDouble = return (t2, op (IntDouble e1) e2)
                       | t1 == TDouble && t2 == TInt = return (t1, op e1 (IntDouble e2))
                       | otherwise = do errorMsg $ "Erro de tipos na expressao: " ++ show (op e1 e2) ++ ", " ++
                                                   show e1 ++ " eh do tipo " ++ show t1 ++ " e " ++ show e2 ++
                                                   " eh do tipo " ++ show t2 ++" \n"
                                        return (t1, op e1 e2)

-- Toda divisão resulta no tipo double e e2 não pode ser igual a 0
verificaDiv op e1 e2 t1 t2 | e2 /= Const(CInt 0) && e2 /= Const(CDouble 0.0) && t1 /= TInt && t2 /= TInt = coercao Div e1 e2 t1 t2
                | e2 /= Const(CInt 0) && e2 /= Const(CDouble 0.0)                                        = coercao Div (IntDouble e1) (IntDouble e2) t1 t2 -- Emite warning? Tem q fazer IntDouble da coercao
                | otherwise = do errorMsg $ "Erro de input na expressao: " ++ show (op e1 e2) ++ ", " ++
                                            show e2 ++ " nao pode estar no denominador\n"
                                 return (t2, op e1 e2)

-- tfun = tabela de tipos de funcoes e tvar = tabela de tipos de variaveis

tExpr :: t1 -> t2 -> Expr -> Result (Tipo, Expr)
tExpr tfun tvar (Lit x) = return (TString, Lit x)
tExpr tfun tvar (Const (CInt x)) = return (TInt, Const(CInt x))
tExpr tfun tvar (Const (CDouble x)) = return (TDouble, Const(CDouble x))

tExpr tfun tvar (Neg (Const (CInt 0))) = return (TInt, Const(CInt 0))
tExpr tfun tvar (Neg (Const (CInt x))) = return (TInt, Neg(Const(CInt x)))
tExpr tfun tvar (Neg (Const (CDouble 0))) = return (TDouble, Const(CDouble 0))
tExpr tfun tvar (Neg (Const (CDouble x))) = return (TDouble, Neg(Const(CDouble x)))

tExpr tfun tvar (Add e1 e2) = do (t1, e1') <- tExpr tfun tvar e1
                                 (t2, e2') <- tExpr tfun tvar e2
                                 coercao Add e1' e2' t1 t2
tExpr tfun tvar (Sub e1 e2) = do (t1, e1') <- tExpr tfun tvar e1
                                 (t2, e2') <- tExpr tfun tvar e2
                                 coercao Sub e1' e2' t1 t2
tExpr tfun tvar (Mul e1 e2) = do (t1, e1') <- tExpr tfun tvar e1
                                 (t2, e2') <- tExpr tfun tvar e2
                                 coercao Mul e1' e2' t1 t2

tExpr tfun tvar (Div e1 e2) = do (t1, e1') <- tExpr tfun tvar e1
                                 (t2, e2') <- tExpr tfun tvar e2
                                 verificaDiv Div e1' e2' t1 t2                              

