module Gera_codigo where

import Control.Monad.State
import System.IO
import Ri
import qualified Lex as L

novoLabel::State Int String
novoLabel = do {n <- get; put (n+1); return ("l"++show n)}

genCab nome = return (".class public " ++ nome ++
                      "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokespecial java/lang/Object/<init>()V\n\treturn\n.end method\n\n")

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
genExpr c tab fun (IdVar s) = return (verificaTabTipo tab s)
genExpr c tab fun (Chamada id lista_expr) = genCall c tab fun id lista_expr
genExpr c tab fun (Neg e) = do (t, e') <- genExpr c tab fun e
                               return (t, e' ++ genOp t "neg")

genCast c tab fun op e = do (t, e') <- genExpr c tab fun e
                            case op e of
                                (IntDouble e) -> return (TDouble, e' ++ "\ti2d\n")
                                (DoubleInt e) -> return (TInt, e' ++ "\td2i\n")

genExprAux c tab fun op e1 e2 = do (t1, e1') <- genExpr c tab fun e1;
                                   (t2, e2') <- genExpr c tab fun e2;
                                   case op e1 e2 of
                                    (Add e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "add\n")
                                    (Sub e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "sub\n")
                                    (Mul e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "mul\n")
                                    (Div e1 e2) -> return (t1, e1' ++ e2' ++ genOp t1 "div\n")

genInt n | n <= 5 = "\ticonst " ++ show n ++ "\n"
         | n <= 127 = "\tbipush " ++ show n ++ "\n"
         | otherwise = "\tldc " ++ show n ++ "\n"

genDouble n = "\tldc " ++ show n ++ "\n"

genString n = "\tldc " ++ show n ++ "\n"

verificaTabTipo [] nome = (TVoid, "")
verificaTabTipo tvar@(id :#: (tipo, endereco):xs) nome = if nome == id then (tipo, genLoad tipo endereco)
                                                         else verificaTabTipo xs nome

genLoad tipo endereco = case tipo of
                            TString -> "\taload " ++ show endereco ++ "\n"
                            TInt -> if endereco >= 0 && endereco <= 5 then "\tiload_" ++ show endereco ++ "\n"
                                    else if endereco >= 6 && endereco <= 255 then "\tiload " ++ show endereco ++ "\n"
                                    else "\twide\n"++"\tiload " ++ show endereco ++ "\n"
                            TDouble -> if endereco >= 0 && endereco <= 5 then "\tdload_" ++ show endereco ++ "\n"
                                       else if endereco >= 6 && endereco <= 255 then "\tdload " ++ show endereco ++ "\n"
                                       else "\twide\n"++"\tdload " ++ show endereco ++ "\n"

genOp t op = case t of
               TInt -> "\ti" ++ op
               TDouble -> "\td" ++ op
            --    _ -> "Erro na geracao de codigo: operacao " ++ show op ++ " nao suporta o tipo " ++ show t

genCall c tab fun id lista_expr = do (t, call) <- verificaFun c fun id
                                     lista_expr' <- empilhaExpr c tab fun id lista_expr
                                     return (t, lista_expr' ++ call)

verificaFun _ [] _ = return (TVoid, "")
verificaFun c fun@(id :->: (parametros, tipo):xs) nome = if nome == id then return (tipo, "\tinvokestatic " ++ c ++" /" ++ nome ++ "(" ++  verificaTipos parametros ++ ")" ++ genTipo tipo ++ "\n")
                                                       else verificaFun c xs nome

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
                                            (Req e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "eq" ++ "\tgoto " ++ f ++ "\n")
                                            (Rdif e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "ne" ++ "\tgoto " ++ f ++ "\n")
                                            (Rlt e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "lt" ++ "\tgoto " ++ f ++ "\n")
                                            (Rle e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "le" ++ "\tgoto " ++ f ++ "\n")
                                            (Rgt e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "gt" ++ "\tgoto " ++ f ++ "\n")
                                            (Rge e1 e2) -> return (e1' ++ e2'++ genRel t1 t2 v "ge" ++ "\tgoto " ++ f ++ "\n")

genRel t1 t2 label_v op = case (t1, t2) of
                              (TInt, TInt) -> "\tif_icmp" ++ op ++ " " ++ label_v ++ "\n"
                              (TString, TString) -> "\tif_acmp" ++ op ++ " " ++ label_v ++ "\n"
                              (TDouble, TDouble) -> "\tdcmpg\nif" ++ op ++ " " ++ label_v ++ "\n"
                              -- _ -> "Erro na comparação: argumentos de tipos diferentes"
genExprL c tab fun v f (And e1 e2) = do l1 <- novoLabel
                                        e1' <- genExprL c tab fun l1 f e1
                                        e2' <- genExprL c tab fun v f e2
                                        return (e1'++ l1 ++ ":\n" ++ e2')
genExprL c tab fun v f (Or e1 e2) = do l1 <- novoLabel
                                       e1' <- genExprL c tab fun v l1 e1
                                       e2' <- genExprL c tab fun v f e2
                                       return (e1' ++ l1 ++ ":\n" ++ e2')
genExprL c tab fun v f (Rel e) = genExprR c tab fun v f e
genExprL c tab fun v f (Not e) = genExprL c tab fun f v e

genBloco _ _ _ [] = return ""
genBloco c tab fun lista_comandos@(cmd:xs) = do cmd' <- genCmd c tab fun cmd
                                                resto <- genBloco c tab fun xs
                                                return (cmd' ++ resto)

genCmd c tab fun (Ret e) = case e of
                               Nothing -> return (genTipoReturn TVoid)
                               Just e -> do (t, e') <- genExpr c tab fun e
                                            return (e' ++ genTipoReturn t)
genCmd c tab fun (Imp e) = do (t,e') <- genExpr c tab fun e;
                              (d,s) <- return (TVoid, "\tgetstatic java/lang/System/out LJava/io/PrintStream;\n");
                              (d,s) <- return (TVoid, s ++ e');
                              (d,s) <- return (TVoid, s ++ ("\tinvokevirtual java/io/PrintStream/println("++genTipo t++")V\n"));
                              return s
genCmd c tab fun (Atrib id expr) = do (t, expr') <- genExpr c tab fun expr
                                      let (nome :#: (tipo, endereco)) = verificaTab tab id
                                      return (expr' ++ genStore t endereco)
genCmd c tab fun (If e bloco []) = do lv <- novoLabel
                                      lf <- novoLabel
                                      e' <- genExprL c tab fun lv lf e
                                      b' <- genBloco c tab fun bloco
                                      return (e' ++ lv ++ ":\n" ++ b' ++ lf ++ ":\n")
genCmd c tab fun (If e blocoThen blocoElse) = do lv <- novoLabel
                                                 lf <- novoLabel
                                                 lend <- novoLabel
                                                 e' <- genExprL c tab fun lv lf e
                                                 bThen' <- genBloco c tab fun blocoThen
                                                 bElse' <- genBloco c tab fun blocoElse
                                                 return (e' ++ lv ++ ":\n" ++ bThen' ++ "\tgoto " ++ lend ++ "\n" ++ lf ++ ":\n" ++ bElse' ++ "\tgoto " ++ lend ++ "\n" ++ lend ++ ":\n")
genCmd c tab fun (While e b) = do li <- novoLabel
                                  lv <- novoLabel
                                  lf <- novoLabel
                                  e' <- genExprL c tab fun lv lf e
                                  b' <- genBloco c tab fun b
                                  return (li++":\n"++e'++lv++":\n"++b'++"\tgoto "++li++"\n"++lf++":\n")
genCmd c tab fun (Proc id lista_expr) = do (tipo, lista_expr') <- genExpr c tab fun (Chamada id lista_expr)
                                           let pop = if tipo /= TVoid then "\tpop\n" else "" -- O output de um proc nunca é utilizado, se for utilizado é uma chamada
                                           return (lista_expr' ++ pop)

genTipoReturn t = case t of
                      TInt -> "\tireturn\n"
                      TDouble -> "\tdreturn\n"
                      TString -> "\tareturn\n"
                      TVoid -> "\treturn\n"

verificaTab [] _ = ("" :#: (TVoid, -1))
verificaTab tab@(var@(id :#: (tipo, endereco)):xs) nome = if nome == id then var
                                                          else verificaTab xs nome

genStore tipo endereco = case tipo of
                            TString -> "\tastore " ++ show endereco ++ "\n"
                            TInt -> if endereco >= 0 && endereco <= 5 then "\tistore_" ++ show endereco ++ "\n"
                                    else if endereco >= 6 && endereco <= 255 then "\tistore " ++ show endereco ++ "\n"
                                    else "\twide\n"++"\tistore " ++ show endereco ++ "\n"
                            TDouble -> if endereco >= 0 && endereco <= 5 then "\tdstore_" ++ show endereco ++ "\n"
                                       else if endereco >= 6 && endereco <= 255 then "\tdstore " ++ show endereco ++ "\n"
                                       else "\twide\n"++"\tdstore " ++ show endereco ++ "\n"

genFunc c tab fun f@(id :->: (parametros, tipo)) bloco = do b' <- genBloco c tab fun bloco
                                                            return (".method public static " ++ id ++ "(" ++ verificaTipos parametros ++ ")" ++ genTipo tipo ++ "\n"
                                                                   ++ "\t.limit stack 50\n"
                                                                   ++ "\t.limit locals 8\n"
                                                                   ++ b'
                                                                   ++ ".end method\n\n")


genFuncoes _ [] [] = return ""
genFuncoes c fun@(f:xs) escopos@((id, vars, bloco):ys) = do f' <- genFunc c vars fun f bloco
                                                            resto <- genFuncoes c xs ys
                                                            return (f' ++ resto)

genProg c (Prog fun escopos vars_main bloco_principal) = do cabecalho <- genCab c
                                                            funcoes <- genFuncoes c fun escopos
                                                            cabecalhoMain <- genMainCab 50 8 -- chutei
                                                            bloco_principal' <- genBloco c vars_main fun bloco_principal
                                                            return (cabecalho ++ funcoes ++ cabecalhoMain ++ bloco_principal' ++ ".end method\n\n")

gen nomePrograma ast = evalState (genProg nomePrograma ast) 0

-- tab: ["x" :#: (TInt, 0), "nome_user" :#: (TString, 0), "precisao" :#: (TDouble, 0)]
-- tfun: ["fat" :->: (["n" :#: (TInt, 0), "nome" :#: (TString, 0), "precisa" :#: (TDouble, 0)], TInt)]