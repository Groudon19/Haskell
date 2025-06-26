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
verificaDiv op e1 e2 t1 t2 | e2 /= Const (CInt 0) && e2 /= Const (CDouble 0.0) && t1 /= TInt && t2 /= TInt = coercao Div e1 e2 t1 t2
                           | e2 /= Const (CInt 0) && e2 /= Const (CDouble 0.0)                             = coercao Div (IntDouble e1) (IntDouble e2) t1 t2 -- Emite warning? Tem q fazer IntDouble da coercao
                           | otherwise = do errorMsg $ "Erro de input na expressao: " ++ show (op e1 e2) ++ ", " ++
                                                       show e2 ++ " nao pode estar no denominador\n"
                                            return (t2, op e1 e2)

tvarConsultaTipo :: [Var] -> Id -> Result Tipo
tvarConsultaTipo [] nome = do errorMsg $ "Variavel '" ++ nome ++ "' nao encontrada\n"
                              return TVoid
tvarConsultaTipo tvar@(id :#: (tipo, valor):xs) nome = if id == nome then return tipo else tvarConsultaTipo xs nome

tfunConsultaTipo :: [Funcao] -> Id -> Result Tipo
tfunConsultaTipo [] nome = do errorMsg $ "Funcao '" ++ nome ++ "' nao encontrada\n"
                              return TVoid
tfunConsultaTipo tfun@(id :->: (parametros, tipo):xs) nome = if id == nome then return tipo else tfunConsultaTipo xs nome

tfunConsultaParametros :: [Funcao] -> Id -> [Var] -- Pode retornar Result?
tfunConsultaParametros tfun@(id :->: (parametros, tipo):xs) nome = if id == nome then parametros else tfunConsultaParametros xs nome

verificaParametros :: [Funcao] -> [Var] -> [Var] -> [Expr] -> Bool
verificaParametros tfun tvar [] [] = True
verificaParametros tfun tvar [] _ = False
verificaParametros tfun tvar _ [] = False
verificaParametros tfun tvar parametros@(id :#: (tipo,valor):xs) variaveis@(y:ys) = case tExpr tfun tvar y of Result (_, _, (t, _)) -> (tipo == t) && verificaParametros tfun tvar xs ys

-- tfun = tabela de tipos de funcoes e tvar = tabela de tipos de variaveis
-- tvar: ["x" :#: (TInt, 0), "nome_user" :#: (TString, 0), "precisao" :#: (TDouble, 0)]
-- tfun: ["fat" :->: (["n" :#: (TInt, 0), "nome" :#: (TString, 0), "precisa" :#: (TDouble, 0)], TInt)]

tExpr :: [Funcao] -> [Var] -> Expr -> Result (Tipo, Expr)
tExpr tfun tvar (Lit x) = return (TString, Lit x)

tExpr tfun tvar (IdVar x) =  do t <- tvarConsultaTipo tvar x
                                return (t, IdVar x)

tExpr tfun tvar (Chamada id parametros) = do if verificaParametros tfun tvar (tfunConsultaParametros tfun id) parametros
                                             then do t <- tfunConsultaTipo tfun id
                                                     return (t, Chamada id parametros)
                                             else do errorMsg $ "Erro no input da expressao: " ++ show (Chamada id parametros) ++ "\n"
                                                     return (TVoid, Chamada id parametros)

tExpr tfun tvar (Const (CInt x)) = return (TInt, Const (CInt x))
tExpr tfun tvar (Const (CDouble x)) = return (TDouble, Const (CDouble x))

tExpr tfun tvar (Neg (Const (CInt 0))) = return (TInt, Const (CInt 0))
tExpr tfun tvar (Neg (Const (CInt x))) = return (TInt, Neg (Const (CInt x)))
tExpr tfun tvar (Neg (Const (CDouble 0))) = return (TDouble, Const (CDouble 0))
tExpr tfun tvar (Neg (Const (CDouble x))) = return (TDouble, Neg (Const (CDouble x)))

tExpr tfun tvar (IntDouble x) = do (t1, e1') <- tExpr tfun tvar x
                                   if (t1 == TInt) then do return (TDouble, e1') -- IntDouble e1' parece virar um loop infinito
                                   else if (t1 == TDouble) then do return (t1, e1')
                                   else do errorMsg $ "Erro de tipos na expressao: " ++ show (IntDouble x) ++ ", " ++
                                                      show t1 ++ " nao pode virar double\n"
                                           return (t1, e1')

tExpr tfun tvar (DoubleInt x) = do (t1, e1') <- tExpr tfun tvar x
                                   if (t1 == TDouble) then do return (TInt, e1')
                                   else if (t1 == TInt) then do return (t1, e1')
                                   else do errorMsg $ "Erro de tipos na expressao: " ++ show (DoubleInt x) ++ ", " ++
                                                      show t1 ++ " nao pode virar int\n"
                                           return (t1, e1')

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

tExprR tfun tvar (Req e1 e2) = tExpR tfun tvar Req e1 e2
tExprR tfun tvar (Rdif e1 e2) = tExpR tfun tvar Rdif e1 e2
tExprR tfun tvar (Rle e1 e2) = tExpR tfun tvar Rle e1 e2
tExprR tfun tvar (Rge e1 e2) = tExpR tfun tvar Rge e1 e2
tExprR tfun tvar (Rlt e1 e2) = tExpR tfun tvar Rlt e1 e2
tExprR tfun tvar (Rgt e1 e2) = tExpR tfun tvar Rgt e1 e2

tExpR tfun tvar op e1 e2 = do (t1, e1') <- tExpr tfun tvar e1
                              (t2, e2') <- tExpr tfun tvar e2
                              if t1 == t2 then return (op e1' e2')
                              else case (t1, t2) of
                                     (TString, _) -> do errorMsg $ "Nao da pra comparar String com " ++ show t2 ++ "\n"
                                                        return (op e1' e2')
                                     (_, TString) -> do errorMsg $ "Nao da pra comparar String com " ++ show t1 ++ "\n"
                                                        return (op e1' e2')
                                     (TInt, TDouble) -> do warningMsg $ "Realizando a coercao de " ++ show t1 ++ " para " ++ show t2 ++ " de " ++ show e1' ++ "\n"
                                                           return (op (IntDouble e1') e2')
                                     (TDouble, TInt) -> do warningMsg $ "Realizando a coercao de " ++ show t2 ++ " para " ++ show t1 ++ " de " ++ show e2' ++ "\n"
                                                           return (op e1' (IntDouble e2'))
                                     _ -> do errorMsg $ "Impossivel comparar "++ show t1 ++ " com " ++ show t2 ++ "\n"
                                             return (op e1' e2')