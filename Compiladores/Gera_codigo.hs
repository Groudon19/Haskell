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




genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Const (CDouble d)) = return (TDouble, genDouble d)
genExpr c tab fun (Lit s) = return (TString, genString s)
genExpr c tab fun (IntDouble e) = genCast c tab fun IntDouble e
genExpr c tab fun (DoubleInt e) = genCast c tab fun DoubleInt e
genExpr c tab fun (Add e1 e2) = genExprAux c tab fun Add e1 e2
genExpr c tab fun (Sub e1 e2) = genExprAux c tab fun Sub e1 e2
genExpr c tab fun (Mul e1 e2) = genExprAux c tab fun Mul e1 e2
genExpr c tab fun (Div e1 e2) = genExprAux c tab fun Div e1 e2
genExpr c tab fun (IdVar s) = return (verificaTab tab s)
genExpr c tab fun (Chamada id lista_expr) = genCall c tab fun id lista_expr
genExpr c tab fun (Neg e) = do (t, e') <- genExpr c tab fun e
                               return (t, e' ++ genOp t "neg")

genCast c tab fun op e = do (t, e') <- genExpr c tab fun e
                            case op e of
                                (IntDouble e) -> return (TDouble, e' ++ "i2d\n")
                                (DoubleInt e) -> return (TInt, e' ++ "d2i\n")

genExprAux c tab fun op e1 e2 = do (t1, e1') <- genExpr c tab fun e1;
                                   (t2, e2') <- genExpr c tab fun e2;
                                   case op e1 e2 of
                                    (Add e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "add")
                                    (Sub e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "sub")
                                    (Mul e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "mul")
                                    (Div e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "div")

genInt n | n <= 5 = "iconst " ++ show n ++ "\n"
         | n <= 127 = "bipush " ++ show n ++ "\n"
         | otherwise = "ldc " ++ show n ++ "\n"

genDouble n = "ldc " ++ show n ++ "\n"

genString n = "ldc " ++ show n ++ "\n"

verificaTab [] nome = (TVoid, "")
verificaTab tvar@(id :#: (tipo, endereco):xs) nome = if nome == id then (tipo, genLoad tipo endereco)
                                                     else verificaTab xs nome

genLoad tipo endereco = case tipo of
                            TString -> "aload " ++ show endereco ++ "\n"
                            TInt -> if endereco >= 0 && endereco <= 5 then "iload_" ++ show endereco ++ "\n"
                                    else if endereco >= 6 && endereco <= 255 then "iload " ++ show endereco ++ "\n"
                                    else "wide\n"++"iload " ++ show endereco ++ "\n"
                            TDouble -> if endereco >= 0 && endereco <= 5 then "dload_" ++ show endereco ++ "\n"
                                       else if endereco >= 6 && endereco <= 255 then "dload " ++ show endereco ++ "\n"
                                       else "wide\n"++"dload " ++ show endereco ++ "\n"

genOp t op = case t of
               TInt -> "i" ++ op
               TDouble -> "d" ++ op
            --    _ -> "Erro na geracao de codigo: operacao " ++ show op ++ " nao suporta o tipo " ++ show t

genCall c tab fun id lista_expr = do (t, call) <- verificaFun fun id
                                     lista_expr' <- empilhaExpr c tab fun id lista_expr
                                     return (t, lista_expr' ++ call)

verificaFun [] _ = return (TVoid, "")
verificaFun fun@(id :->: (parametros, tipo):xs) nome = if nome == id then return (tipo, "invokestatic " ++ nome ++ "(" ++  verificaTipos parametros ++ ")" ++ show tipo)
                                                       else verificaFun xs nome

verificaTipos [] = ""
verificaTipos tab@(id :#: (tipo, endereco):xs) = genTipo tipo ++ verificaTipos xs

genTipo t = case t of
                TInt -> "I"
                TDouble -> "D"
                TString -> "Ljava/lang/String;"
                TVoid -> "V"

empilhaExpr _ _ _ _ [] = return ""
empilhaExpr c tab fun id (expr:xs) = do (t, expr') <- genExpr c tab fun expr
                                        resto <- empilhaExpr c tab fun id xs
                                        return (expr' ++ resto)

genExprR c tab fun v f (Req e1 e2) = genExprRAux c tab fun v f Req e1 e2
genExprR c tab fun v f (Rdif e1 e2) = genExprRAux c tab fun v f Rdif e1 e2
genExprR c tab fun v f (Rlt e1 e2) = genExprRAux c tab fun v f Rlt e1 e2
genExprR c tab fun v f (Rle e1 e2) = genExprRAux c tab fun v f Rle e1 e2
genExprR c tab fun v f (Rgt e1 e2) = genExprRAux c tab fun v f Rgt e1 e2
genExprR c tab fun v f (Rge e1 e2) = genExprRAux c tab fun v f Rge e1 e2

genExprRAux c tab fun v f op e1 e2 = do (t1, e1') <- genExpr c tab fun e1
                                        (t2, e2') <- genExpr c tab fun e2
                                        case op e1 e2 of
                                            (Req e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "eq" ++ "\tgoto " ++ show f ++ "\n")
                                            (Rdif e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "ne" ++ "\tgoto " ++ show f ++ "\n")
                                            (Rlt e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "lt" ++ "\tgoto " ++ show f ++ "\n")
                                            (Rle e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "le" ++ "\tgoto " ++ show f ++ "\n")
                                            (Rgt e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "gt" ++ "\tgoto " ++ show f ++ "\n")
                                            (Rge e1 e2) -> return(e1' ++ e2'++ genRel t1 t2 v "ge" ++ "\tgoto " ++ show f ++ "\n")

genRel t1 t2 label_v op = case (t1, t2) of
                              (TInt, TInt) -> "if_icmp" ++ op ++ " " ++ show label_v ++ "\n"
                              (TDouble, TDouble) -> "dcmpg\nif" ++ op ++ " " ++ show label_v ++ "\n"
                              -- _ -> "Erro na comparação: argumentos de tipos diferentes"
genExprL c tab fun v f (And e1 e2) = do l1 <- novoLabel
                                        e1' <- genExprL c tab fun l1 f e1
                                        e2' <- genExprL c tab fun v f e2
                                        return (e1'++l1++":\n"++e2')
genExprL c tab fun v f (Or e1 e2) = do l1 <- novoLabel
                                       e1' <- genExprL c tab fun v l1 e1
                                       e2' <- genExprL c tab fun v f e2
                                       return(e1' ++ l1 ++ ":\n" ++ e2')
genExprL c tab fun v f (Rel e) = genExprR c tab fun v f e
genExprL c tab fun v f (Not e) = genExprL c tab fun f v e

-- genCmd c tab fun (While e b) = do {li <- novoLabel; lv <- novoLabel; lf <- novoLabel; e' <- genExprL c tab fun lv lf e; b' <- genBloco c tab fun b; return (li++":\n"++e'++lv++":\n"++b'++"\tgoto "++li++"\n"++lf++":\n")}
-- -- todo


-- tab: ["x" :#: (TInt, 0), "nome_user" :#: (TString, 0), "precisao" :#: (TDouble, 0)]
-- tfun: ["fat" :->: (["n" :#: (TInt, 0), "nome" :#: (TString, 0), "precisa" :#: (TDouble, 0)], TInt)]