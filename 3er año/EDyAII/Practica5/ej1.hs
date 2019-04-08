

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show     


(|||)::a -> b -> (a,b)
a ||| b = (a,b)


size::BTree a -> Int
size Empty = 0
size (Node n l x r) = n


nth::BTree a -> Int -> a
nth (Node n1 l x r) n | n < pos = nth l n
                      | n > pos = nth r (n1-pos) --Calcular n1-pos-esimo en árbol derecho 
                      | n == pos = x
                      where pos = (size l) + 1



cons::a->BTree a->BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node n l y r) = Node (n+1) (cons x l) y r

tabulate::(Int -> a)->Int->BTree a
tabulate f n = tabulate' f n n
    where
        tabulate' f n c | n > 0 && c > 0 = let m = div n 2
                                               (l',r') = (tabulate' f m m) ||| (tabulate' f n (n-m-1))
                                  in Node n l' (f (n-(size r'))) r'
                        |otherwise = Empty
  


mapT::(a->b)->BTree a->BTree b
mapT f Empty = Empty
mapT f (Node n l x r) = let (l2,r2) = (mapT f l) ||| (mapT f r)
                        in
                        Node n l2 (f x) r2

takeT::Int->BTree a->BTree a
takeT 0 _ = Empty
takeT n Empty = Empty
takeT n (Node n1 l x r) | n < cantPrimerosEltos = takeT n l
                        | n > cantPrimerosEltos= Node n1 l x (takeT (n-cantPrimerosEltos) r)
                        | n == cantPrimerosEltos = (Node cantPrimerosEltos l x Empty)
                        where cantPrimerosEltos = (size l)+1


dropT::Int->BTree a->BTree a
dropT 0 t = t
dropT n Empty = Empty
dropT n (Node n1 l x r) | n < cantPrimerosEltos = Node (n1-n) (dropT n l) x r
                        | n > cantPrimerosEltos = dropT (n-cantPrimerosEltos) r
                        | n == cantPrimerosEltos = r
                        where cantPrimerosEltos = (size l) + 1




tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty 
tabulate f n = Node n (tabulate f m) (f m) (tabulate (f . (+ (m+1))) (n-(m+1)) ) where
  m = (div n 2)