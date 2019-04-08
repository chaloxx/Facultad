data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show  

type Tupla a = (BTree a, BTree a)

(|||)::a -> b -> (a,b)
a ||| b = (a,b)

cons::a->BTree a->BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node n l y r) = Node (n+1) (cons x l) y r





size::BTree a -> Int
size Empty = 0
size (Node n l x r) = n


takeT::Int->BTree a->BTree a
takeT 0 _ = Empty
takeT n Empty = Empty
takeT n (Node n1 l x r) | n < cantPrimerosEltos = takeT n l
                        | n > cantPrimerosEltos = Node n l x (takeT (n-cantPrimerosEltos) r)
                        | n == cantPrimerosEltos = (Node cantPrimerosEltos l x Empty)
                        where cantPrimerosEltos = (size l)+1


dropT::Int->BTree a->BTree a
dropT 0 t = t
dropT n Empty = Empty
dropT n (Node n1 l x r) | n < cantPrimerosEltos = Node (n1-n) (dropT n l) x r
                        | n > cantPrimerosEltos = dropT (n-cantPrimerosEltos) r
                        | n == cantPrimerosEltos = r
                        where cantPrimerosEltos = (size l) + 1




splitAtT::BTree a -> Int -> Tupla a
splitAtT t 0 = (Empty,t)
splitAtT t n | n <= (size t) = (takeT n t) ||| (dropT n t)




rebalance::BTree a -> BTree a 
rebalance Empty = Empty
rebalance t@(Node n lt x rt) | diff (size lt) (size rt) = let (lt',rt') = (rebalance lt) ||| (rebalance rt)
                                                   in Node n lt' x rt'

                             | otherwise = let (lt',rt') = splitAtT t (div n 2)
                                               ((Node 1 Empty y Empty), rt'') = splitAtT rt' 1
                                               (lt2,rt2) =  (rebalance lt') ||| (rebalance rt'')
                                         in Node n lt2 y rt2
                              where diff n m = abs (n-m) <= 1



prueba = Node 5 Empty 1 (Node 4 Empty 2 (Node 3 Empty 3 (Node 2 Empty 4 (Node 1 Empty 5 Empty))))


prueba2 = Node 4 Empty 1 (Node 3 Empty 2 (Node 2 Empty 3 (Node 1 Empty 4 Empty)))



Node 6 (Node 3 (Node 1 Empty 1 Empty) 2 (Node 1 Empty 3 Empty)) 4 (Node 1 Empty 2 Empty)
