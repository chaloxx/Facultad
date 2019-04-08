import Control.Exception.Base -- Funcion assert para testing
data Nat =  Zero | Succ Nat deriving Show




add:: Nat -> Nat -> Nat
add n Zero = n
add n  (Succ m)  = add (Succ n) m

mult:: Nat -> Nat -> Nat
mult n Zero = Zero
mult n (Succ m) = add n (mult n m)


{-Un color queda reprensentado por 3 float que representan la proporcion de los
colores base que se uso para formarlo: rojo, verde y azul (en ese orden)-}
type Color = (Float, Float, Float)

mezclar:: Color -> Color -> Color
mezclar (x, y, z) (x', y',z') = (promedio x x', promedio y y', promedio z z')
  where promedio a b = (a+b)/2

{-Una linea queda representada por un string y un entero que denota la posicion
del cursor-}
type Linea = (String,Int)

vacia:: Linea
vacia = ("",0)

moverIzq::Linea -> Linea
moverIzq (s,0) = (s,0)
moverIzq (s,n) = (s,(n-1))

moverDer::Linea -> Linea
moverDer (s,n) | n < (length s) = (s,(n+1))
               | otherwise = (s,n)

moverInit:: Linea -> Linea
moverInit (s,n) = (s,0)

moverFin:: Linea -> Linea
moverFin (s,n) = (s,length s)

insertar:: Linea -> Char -> Linea
insertar (s,n) c = let (xs,xs') = ((take n s),(drop n s)) 
                   in (xs++[c]++xs',n+1)

borrar:: Linea -> Linea
borrar (s,0) = (s,0)
borrar ("",n) = ("",n)
borrar (s,n) = let (xs,xs') = (take (n-1) s, drop n s)
               in (xs++xs',(n-1))
               


data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show


cons:: CList a -> a -> CList a
cons EmptyCL x = CUnit x
cons (CUnit y) x = Consnoc x EmptyCL y
cons (Consnoc x xs y) x' = Consnoc x' (cons xs x) y


snoc:: CList a -> a -> CList a
snoc EmptyCL x = CUnit x
snoc (CUnit y) x = Consnoc y EmptyCL x
snoc (Consnoc x xs y) x' = Consnoc x (snoc xs y) x'


headCL:: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x xs y) = x


tailCL:: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x (CUnit z) y) = Consnoc z EmptyCL y
tailCL (Consnoc x xs y) = let (x',xs') = (headCL xs,tailCL xs) 
                          in Consnoc x' xs' y
 

isEmptyCL:: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL xs = False

isCUnit::  CList a -> Bool
isCUnit (CUnit x) = True
isCUnit xs = False



reverseCL:: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = (CUnit x)
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x



initsCL:: CList a -> CList (CList a)
initsCL EmptyCL = EmptyCL
initsCL (CUnit x) = Consnoc EmptyCL EmptyCL  (CUnit x)
initsCL t@(Consnoc x xs y) = let  xs' = cons xs x
                                  xs'' = initsCL xs'
                             in snoc xs'' t


lastsCL:: CList a -> CList (CList a)
lastsCL EmptyCL = EmptyCL 
lastsCL t@(CUnit x) = Consnoc t EmptyCL EmptyCL
lastsCL t = let  xs' = tailCL t
                 xs'' = initsCL xs'
            in cons xs'' t


concatCL:: CList a -> CList a -> CList a
concatCL EmptyCL ys = ys
concatCL (CUnit x) ys = cons ys x
concatCL t ys = let (x,xs) = (headCL t, tailCL t)
                in cons (concatCL xs ys) x



data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving Show


eval::Aexp -> Int
eval (Num x) = x
eval (Prod x y) = let (res1,res2) = (eval x,eval y)
                        in res1 * res2
eval (Div x y) = let (res1,res2) = (eval x,eval y)
                 in div res1 res2



seval::Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = case (seval x, seval y) of
                   (Just x', Just y') -> Just (x'*y')
                   otherwise -> Nothing
seval (Div x y) = case (seval x, seval y) of
                   (_,Just 0) -> Nothing 
                   (Just x', Just y') -> Just (div x' y')
                   otherwise -> Nothing

data Bin a  = Empty |Nodo (Bin a) a (Bin a) deriving Show

completo::Int-> a -> Bin a
completo 0 x = Nodo Empty x Empty
completo n x = let t = completo (n-1) x
               in  Nodo t x t

balanceado:: Int -> a -> Bin a
balanceado 0 x = Empty
balanceado 1 x = Nodo Empty x Empty
balanceado n x = let m = div n 2
                     t = balanceado m x
                 in case even n of
                    False -> Nodo t x (Nodo Empty x t)
                    True -> Nodo t x t 


data GenTree a = EmptyG | NodeG a [GenTree a ] deriving Show
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a) deriving Show



g2bt:: GenTree a -> BinTree a
g2bt EmptyG = EmptyB
g2bt (NodeG x xs) = NodeB (g2bt' xs) x EmptyB

g2bt'::[GenTree a] -> BinTree a
g2bt' [] = EmptyB
g2bt' (x:ys) = let ((NodeB lt x' EmptyB),rt) = (g2bt x,g2bt' ys)
               in NodeB lt x' rt

g2bt''::[GenTree a] -> BinTree a
g2bt'' [] = EmptyB
g2bt'' ((NodeG x xs):ys) = NodeB (g2bt'' xs) x (g2bt'' ys)


elto = NodeG 1 [(NodeG 2 []),(NodeG 3 []),(NodeG 4 [])]
elto2 = NodeG 1 [(NodeG 2 [(NodeG 3 [])]),(NodeG 4 []),(NodeG 5 [])]








data Trie = E | N String [Trie] deriving (Eq, Show)


isPrefix::String -> String -> Bool
isPrefix "" ys = True
isPrefix xs "" = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys
                         else False



lookUp:: String -> Trie -> Bool
lookUp _  E = False
lookUp "" t = True
lookUp ys (N x xs) | x == ys = True
                   | isPrefix ys x = lookUp' ys xs
                   |otherwise = False
  where lookUp' ys [] =  False
        lookUp' ys (x:xs) = (lookUp ys x)  || (lookUp' ys xs)



insert:: String -> Trie -> Trie
insert ys E = N ys []
insert ys (N x xs) | isPrefix x ys = N x (insertList ys xs)
                   | isPrefix ys x = N ys [(N x xs)]
                   |otherwise = N x xs


insertList:: String -> [Trie] -> [Trie]
insertList ys [] = [(N ys [])]
insertList ys (t@(N x xs):zs) = let (exp1,exp2) = (isPrefix x ys, isPrefix ys x)
                                in if exp1 || exp2 then (insert ys t):zs
                                   else (t:insertList ys zs)



member:: Ord a => a -> Bin a -> Bool
member x Empty = False
member x (Nodo l y r) | x <= y = member' x y l 
                      | otherwise = member x r
    where member' x y Empty = x == y
          member' x y (Nodo l z r) | x <= z = member' x z l
                                   | otherwise = member' x y r 




main = do 
       print (assert ( insert "" E == (N "" [])) "Okey") 
       print (assert ( insert "Abla" (N "" []) == (N "" [(N "Abla" [])])) "Okey") 
       print (assert ( insert "Beta" (N "" [(N "Abla" [])]) == (N "" [(N "Abla" []),(N "Beta" [])])) "Okey") 
       print (assert ( insert "Bet" (N "" [(N "Abla" []),(N "Beta" [])]) == (N "" [(N "Abla" []),(N "Bet" [(N "Beta" [])])])) "Okey")  
       

       print (assert ( member 3 (Nodo (Nodo Empty 2 Empty) 1 (Nodo (Nodo Empty 2 Empty) 3 Empty)) == True) "Okey")