import ListSeq 
import Control.Exception.Base -- Funcion assert para testing
data Tree a = E | L a |  N  Int (Tree a) (Tree a) deriving (Show,Eq)


concatT:: Tree (Tree a) ->  Tree a
concatT E = E
concatT (L x) = x
concatT (N n l r) = let (l',r') = (concatT l, concatT r)
                 in N ((size l') + (size r')) l' r'

size:: Tree a -> Int
size E = 0
size (L x) = 1
size (N r _ _) = r

subsequence:: Int -> Int -> Tree a -> Tree a
subsequence i j E = E
subsequence i j (L x) = if i == j then (L x)
                        else E
subsequence i j (N n l r) = let (limIzq, limDer) = ((size l)-1, (size l + size r)-1)
                            in if i < 0 || j > limDer then E
                               else if i >= (limIzq+1) && j <= limDer then subsequence (i- (size l)) (j- (size l)) r
                               else if i >= 0 && j <= limIzq then subsequence i j l
                               else let (l',r') = (subsequence i limIzq l,subsequence 0 (j - (size l)) r)
                                    in N ((size l') + (size r')) l' r'




uniquify:: [Int] -> [Int]
uniquify [] = []
uniquify (x:xs) = let f' y = y /= x
                      xs' = filterS f' xs
                  in (x:(uniquify xs'))


type Intervalo a = ((Int,a),(Int,a))

tabulate::(Int -> a) -> Int -> [a]
tabulate  f n | n == 1 = [f 0]
              | n > 1 = let m = div n 2
                            (lx,rx) = ((tabulate f m),tabulate (f.(+ m)) (n-m))
                        in  appendS lx rx



{-sccml::[Int] -> Int
sccml s = let s' = tabulate f (lengthS s)
              (s'',r) = scanS combine neutro s'
              w = appendS (dropS s'' 1) (singletonS r)
              w'' = map (\((a,b),(c,d)) -> c-a) w
          in reduceS max 0 w''
    where f i = ((i , nthS s i),(i, nthS s i))
          neutro = ((-2,0),(-2,0))-}










sccml::[Int] -> Int
sccml s = let l = lengthS s
              f i  | i == (l-1) = 0
                   | otherwise = if ((nthS s i) + 1) == (nthS s (i+1)) then 1
                                   else 0
              s' = tabulate f l
              (s'',r) = scanS f' 0 s'
          in reduceS max 0 s''
    where f' x 0 = 0
          f' x y = x + y





aa::[Char] -> Int
aa [] = 0
aa s = let l = lengthS s
           f' i | i == (l-1) = 0
                | otherwise = if (nthS s i) == 'a' && (nthS s (i+1)) == 'a' then 1
                             else 0
           s' = tabulate f' l
       in reduceS (+) 0 s'
    where 


combine::Intervalo Int -> Intervalo Int ->Intervalo Int
combine (p,(x,a)) i@((y,b),p') = if x == (y-1) && a == (b-1) then (p,p')
                                 else i

combine2::Intervalo Char -> Intervalo Char -> Intervalo Char
combine2 (p,(x,a)) i@((y,b),p') = if x == (y-1) && a == 'a' && b == 'a' then (p,p')
                                  else i

{-
aa::[Char] -> [Intervalo Char]
aa s  =let s' = tabulate f (lengthS s)
           (s'',r) = scanS combine2 neutro s'
           w = appendS (dropS s'' 1) (singletonS r)
           w'' = map (\((a,b),(c,d)) -> (c-a)) w
       in s''++[r]
    where f i = ((i , nthS s i),(i, nthS s i))
          neutro = ((-2,'b'),(-2,'b'))
-}


main = do
       print (assert (concatT (N 2 (L (L 3)) (L (L 3))) == (N 2 (L 3) (L 3)))  "Okey")
       
       print (assert ((subsequence 0 3 (N 4 (N 2 (L 0) (L 1)) (N 2 (L 2) (L 3))) == (N 4 (N 2 (L 0) (L 1)) (N 2 (L 2) (L 3)))))  "Okey")
       print (assert ((subsequence 1 2 (N 4 (N 2 (L 0) (L 1)) (N 2 (L 2) (L 3))) == (N 2 (L 1) (L 2)) ))  "Okey")


       print (assert (sccml [3, 5, 1, 2, 3, 4, 5, 6] == 5) "Funca")
       print (assert (sccml [6, 2, 3, 5, 1] == 1) "Funca")
       print (assert (sccml [4, 6, 7, 8, 9, 10] == 4) "Okey")

       --