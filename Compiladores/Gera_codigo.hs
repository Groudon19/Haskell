module Gera_codigo where

import Control.Monad.State
import System.IO
import Ri
import qualified Lex as L

novoLabel::State Int String
novoLabel = do {n <- get; put (n+1); return ("l"++show n)}

genCab nome = return (".class public " ++ nome ++
                      "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n")

genMainCab s l = return (".method public static main([Ljava/lang/String;)V" ++
                         "\n\t.limit stack " ++ show s ++
                         "\n\t.limit locals " ++ show l ++ "\n\n")


genExprL c tab fun v f (And e1 e2) = do {l1 <- novoLabel; e1' <- genExprL c tab fun l1 f e1; e2' <- genExprL c tab fun v f e2; return (e1'++l1++":\n"++e2')}
-- todo

-- genExprR c tab fun v f (Req e1 e2) = do {(t1, e1') <- genExpr c tab fun e1; (t2,e2') <- genExpr c tab fun e2; return (e1'++e2'++genRel t1 t2 v "eq"++"\tgoto "++f++"\n")}
-- -- todo

genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Const (CDouble d)) = return (TDouble, genDouble d)
genExpr c tab fun (Lit s) = return (TString, genString s)
genExpr c tab fun (IntDouble e) = genCast c tab fun IntDouble e
genExpr c tab fun (DoubleInt e) = genCast c tab fun DoubleInt e
genExpr c tab fun (Add e1 e2) = genExprAux c tab fun Add e1 e2
genExpr c tab fun (Sub e1 e2) = genExprAux c tab fun Sub e1 e2
genExpr c tab fun (Mul e1 e2) = genExprAux c tab fun Mul e1 e2
genExpr c tab fun (Div e1 e2) = genExprAux c tab fun Div e1 e2

genCast c tab fun op e = do (t, e') <- genExpr c tab fun e
                            case op e of
                                (IntDouble e) -> return (TDouble, e' ++ " i2d\n")
                                (DoubleInt e) -> return (TInt, e' ++ " d2i\n")

genExprAux c tab fun op e1 e2 = do (t1, e1') <- genExpr c tab fun e1;
                                   (t2, e2') <- genExpr c tab fun e2;
                                   case op e1 e2 of 
                                    (Add e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "add")
                                    (Sub e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "sub")
                                    (Mul e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "mul")
                                    (Div e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "div")


genOp t op = case t of
               TInt -> "i" ++ op
               TDouble -> "d" ++ op
            --    _ -> "Erro na geracao de codigo: operacao " ++ show op ++ " nao suporta o tipo " ++ show t

-- genCmd c tab fun (While e b) = do {li <- novoLabel; lv <- novoLabel; lf <- novoLabel; e' <- genExprL c tab fun lv lf e; b' <- genBloco c tab fun b; return (li++":\n"++e'++lv++":\n"++b'++"\tgoto "++li++"\n"++lf++":\n")}
-- -- todo

genInt n | n <= 5 = "iconst " ++ show n ++ "\n"
         | n <= 127 = "bipush " ++ show n ++ "\n"
         | otherwise = "ldc " ++ show n ++ "\n"

genDouble n = "ldc " ++ show n ++ "\n"

genString n = "ldc " ++ show n ++ "\n"

-- genOp = "i" ou "d" com "\n" no final