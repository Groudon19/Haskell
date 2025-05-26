import Data.Char

--Aula 3: Recursão
fat 0 = 1
fat n = n * fat(n-1)

fat' n = if n == 0 then 1 else n * fat(n-1)

pot n 0 = 1
pot n e = n * pot n (e-1)

pot' n e = if e == 0 then 1 else n * pot' n (e - 1)

mdc a b = if a == b then a else if a > b then mdc (a - b) b else mdc a (b-a)

--Aula 5: Listas
tam [] = 0
tam (x:xs) = 1 + tam xs

somatorio [] = 0 -- 0 é o elemento neutro da operação de soma
somatorio (x:xs) = x + somatorio xs

produtorio [] = 1 -- 1 é o elemento neutro da operação de multiplicação
produtorio (x:xs) = x * produtorio xs

eh [] = True -- True é o elemento neutro da operação && 
eh (x:xs) = x && and xs

soma _ [] = []
soma [] _ = []
soma (x:xs) (y:ys) = x + y : soma xs ys

soma' [] [] = []
soma' a [] = a
soma' [] a = a
soma' (x:xs) (y:ys) = x + y : soma xs ys

maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs --A função "maior xs" é executada duas vezes (ruim)

maior' [x] = x
maior' (x:xs) = if x > m then x else m
    where m = maior xs -- A função "maior xs" é executada só uma vez (bom)

--Aula 6:
pertence e [] = False
pertence e (x:xs) = if x == e then True else pertence e xs

inter [] ys = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys --ignorou o x pq não pertence

concatenar [] (y:ys) = ys
concatenar (x:xs) (y:ys) = x:concatenar xs (y:ys)

insert a [] = [a]
insert a (x:xs) = if a == x then (x:xs) 
                 else if a < x then a:x:xs 
                        else x:insert a xs

insert' a [] = [a]
insert' a l@(x:xs) = if a == x then l 
                 else if a < x then a:l
                        else x:insert a xs --A lista inicial pode ser chamada de l ou x:xs

insert'' e [] = []
insert'' e l@(x:xs)| e == x = l
                | e < x = e:l
                | otherwise = x:insert e xs --Quando se tem mais de dois testes

--Aula 7:
nprimeiros _ [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

inv [] = []
inv (x:xs) = inv xs ++ [x]

--Aula 8: Compreensão de listas
--listas infinitas e lazy evaluation
npares n = nprimeiros n [2*x|x <- [0..]]

remover' _ [] = []
remover' a (x:xs) = if a == x then remover' a xs else x:remover' a xs --Lame way

--remover a xs = [x|x <- xs, x /= a] --Cool way

pot2 n = [2^x|x<-[0..n]]

--Aula 9: Tuplas
primeiro (x,y) = x

segundo (x,y) = y

adicao x y = x + y

adicao' (x,y) = x + y

juntar _ [] = []
juntar [] _ = []
juntar (x:xs) (y:ys) = (x,y) : juntar xs ys

procurar e [] = ""
procurar e ((n,t):ls) = if e == n then t else procurar e ls

tab' _ [] = []
tab' (x:xs) (y:ys) = (x, y, x*y): tab' (x:xs) ys

tab [] _ = []
tab (x:xs) (y:ys) = tab' (x:xs) (y:ys): tab xs (y:ys)

tab'' xs ys = [(x,y,x*y)|x<-xs, y<-ys]               

--Aula 10:
divide [] = ([],[])
divide ((x,y):xys) =(x:xs, y:ys) 
    where (xs,ys) = divide xys

--Aula 12:
aux (x:xs) = if x /= '\n' then [] else x:aux xs
aux' (x:xs) = if x == '\n' then xs else aux' xs

separar' xs = (aux xs, aux' xs)

remover e [] = []
remover e (x:xs) = if x == e then remover e xs else x:remover e xs

removaDup [] = []
removaDup (x:xs) = x:removaDup (remover x xs)

removaDup' [] = []
removaDup' (x:xs) = if pertence x xs then removaDup' xs else x:removaDup' xs --ordem errada

add' (a,b) (c,d) = (a+c, b+d)

separar [] = ([],[])
separar (x:xs) = if x /= '\n' then (x:ys, zs) else ([],xs)
    where (ys,zs) = separar xs

--Aula 13: Funções de primeira ordem
teste1 = (\x y -> x + y) 10

aplica2 f x = f(f x)

flipnoflop f x y = f y x

zero = \s z -> z
um = \s z -> s z
dois = \s z -> s (s z)
tres = \s z -> s (s (s z))
quatro = \s z -> s (s (s (s z)))


suc = \w y x -> y (w y x)
prede = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)
add = \x y w u -> x w (y w u)
mul = \x y w u -> x (y w) u

v = \v f -> v
f = \v f -> f

e = \x y -> x y (\u v -> v)
ou = \x y -> x (\u v -> u) y
nao = \x -> x (\ u v -> v) (\a b -> a)

ehZero = \n -> n (\d -> f) v

teste = \n -> (ehZero n) um dois

--Aula 14: Funções de ordem superior
map' _ [] = []
map' f (x:xs) = f x: map' f xs

map'' f xs = [f x | x <- xs]

filter' _ [] = []
filter' p (x:xs) = if p x then x: filter' p xs else filter' p xs

particao p xs = (filter' p xs,filter' (not.p) xs)

----(.) = \f g x -> f (g x)

--Aula 15:
foldr' f n [] = n --foldright
foldr' f n (x:xs) = f x (foldr' f n xs)

summ = foldr' (+) 0  -- n precisa botar xs, mas pode dar sobrecarga

fatorial n = foldr' (*) 1 [1..n]

tamanho xs = foldr' (\x r -> 1 + r) 0 xs

map''' f ys = foldr' (\s ss -> f s:ss) [] ys --most complex function yet

foldl' f n [] = n --foldleft tem recursão de cauda
foldl' f n (x:xs) =  foldl' f (f n x) xs

--Aula 17: Monadas
newba = putStr "Qual o seu nome?\n" >> getLine >>= (\nome -> putStr ("Hello " ++ nome ++ "\n"))

newba' = do putStr ("Qual o seu nome?\n")
            nome <- getLine
            putStr ("Qual sua idade?\n")
            idade <- getLine
            putStr ("Nome: " ++ nome ++ "\nIdade: " ++ idade ++ "\n")

factorial = do putStr "Numero: "
               num <- getLine
               let n = fat (read num)
               putStr("O resultado eh: " ++ show n ++ "\n")

--Aula 18:
numLines xs = zip [1..] xs --posso omitir o xs

main = do putStr "Arquivo: "
          arq <- getLine
          txt <- readFile arq
          putStr txt
          let nc = length txt
          let nl = length(lines txt)
          putStr ("Numero de caracteres: " ++ show nc ++ "\n")
          putStr ("Numero de linhas: " ++ show nl ++ "\n")
          let linhanum = numLines(lines txt)
          imprimir linhanum

imprimir [] = putStr "\n"
imprimir ((l,s):xs) = do putStr (show l ++ "-")
                         putStrLn s
                         imprimir xs

--aula 19
data Semana = Dom|Seg|Ter|Qua|Qui|Sex|Sab deriving Show


proxDiaSemana Seg = Ter
proxDiaSemana Ter = Qua
proxDiaSemana Qua = Qui
proxDiaSemana Qui = Sex
proxDiaSemana _ = Seg 

data Data = Dia Int Int Int deriving Show

ano (Dia d m a) = a 

data Contato = Pessoa String String Data deriving Show

type Agenda = [Contato]

find nome [] = "nao encontrado"
find nome ((Pessoa n t d):xs) = if n == nome then t else find nome xs

data MinhaLista a = Cons a (MinhaLista a)|Nil deriving Show

--Aula 20: Árvores
data Arvore a = No a (Arvore a)(Arvore a)|Folha deriving Show

ins e Folha = No e Folha Folha
ins e (No x esq dir)|e==x = No x esq dir
                    |e < x = No x (ins e esq) dir
                    |otherwise = No x esq (ins e dir)

insListaArvore::(Ord a) => [a] -> Arvore a
insListaArvore [] = Folha
insListaArvore (x:xs) = ins x (insListaArvore xs)

menorArvore:: Arvore a -> a
menorArvore (No x Folha dir) = x
menorArvore (No x esq dir) = menorArvore esq

arvoreLista Folha = []
arvoreLista (No x esq dir) = arvoreLista esq ++ [x] ++ arvoreLista dir
