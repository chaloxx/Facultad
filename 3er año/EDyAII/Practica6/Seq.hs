{- Implementación del TAD secuencia -}

module Seq where
import Par

class Seq s where
   emptyS     :: s a
   singletonS :: a -> s a
   lengthS    :: s a -> Int 
   nthS       :: s a -> Int -> a 
   tabulateS  :: (Int -> a) -> Int -> s a
   mapS       :: (a -> b) -> s a -> s b 
   filterS    :: (a -> Bool) -> s a -> s a 
   appendS    :: s a -> s a -> s a
   takeS      :: s a -> Int -> s a
   dropS      :: s a -> Int -> s a
   showtS     :: s a -> TreeView a (s a)
   showlS     :: s a -> ListView a (s a)
   joinS      :: s (s a) -> s a
   reduceS    :: (a -> a -> a) -> a -> s a -> a
   scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
   fromListS  :: [a] -> s a
   expandS    ::(a -> a -> a) -> s a -> s a -> s a --Agregada por mi
   contractS  ::(a -> a -> a) -> s a -> s a --Agregada por mi


data TreeView a t = EMPTY | ELT a | NODE t t deriving Show
data ListView a t = NIL | CONS a t deriving Show




instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS [] = 0
    lengthS (x:xs) = 1 + (lengthS xs)
    
    nthS (x:xs) n | n == 0 = x
                  | n > 0 = nthS xs (n-1)

    
   

    mapS f [] = []
    mapS f (x:[]) = [(f x)]
    mapS f (x:xs) = let (x',xs') = (f x) ||| (mapS f xs)
                    in x':xs'

    tabulateS f n | n == 1 = [f 0]
              | n > 1 = let m = div n 2
                            (lx,rx) = (tabulateS f m) ||| tabulateS (f.(+ m)) (n-m)
                        in  appendS lx rx


    filterS f [] = []
    filterS f (x:xs) = let (x',xs') = (f x) ||| (filterS f xs)
                       in 
                       if x' then (x:xs')
                       else xs' 

    appendS [] ys = ys
    appendS (x:xs) ys = x:(appendS xs ys)

    takeS xs 0 = []
    takeS (x:xs) n | n == 1 = [x]
                   | n > 0 =  x:(takeS xs (n-1))

    dropS xs 0 = xs
    dropS (x:xs) n |n == 1 = xs
                   |n > 1 = dropS xs (n-1)

    showtS [] = EMPTY
    showtS (x:[]) = ELT x
    showtS s = let m = div (lengthS s) 2
                   (l,r) = (takeS s m) ||| (dropS s m)
               in NODE  l r

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    
    joinS (x:y:xs) = let (z,zs) =  (appendS x y) ||| (joinS xs)
                     in (appendS z zs)
    joinS (x:[]) = x
    joinS [] = []

    reduceS f b [] = b
    reduceS f b [x] = f b x
    reduceS f b xs = reduceS f b (contractS f xs) 
                                            
                                            
    scanS f b [] = ([],b)
    scanS f b (x:[]) = ([b],(f b x))
    scanS f b xs = let xs' = contractS f xs
                       (s,r) = scanS f b xs'
                   in (expandS f xs s, r) 

    fromListS xs = xs

    contractS f [] = []
    contractS f (x:[]) = [x]
    contractS f (x:y:xs) = let (x',xs') = (f x y) ||| (contractS f xs)
                          in x':xs'

    expandS f [] [] = []
    expandS f (x:[]) (z:zs) = [z]
    expandS f (x:y:xs) (z:zs) = let (x',xs') = (f x z) ||| (expandS f xs zs)
                               in z:x':xs'
                           






