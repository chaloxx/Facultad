

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show


(|||):: a -> b -> (a,b)
a ||| b = (a,b)



gananciaT::Int -> Tree Int -> Tree Int
gananciaT x y =  mapT (\z -> (z-x)) y



mejorGanancia::Tree Int -> Int
mejorGanancia t = maxAll (mapT (\(x,y) -> (gananciaT x y)) (conSufijos t))



mapT::(a -> b)->Tree a-> Tree b
mapT f E = E
mapT f (Leaf x) = Leaf (f x)
mapT f (Join lt rt) = let (lt',rt') = (mapT f lt) ||| (mapT f rt)
                             in
                             Join lt' rt'




sufijos::Tree Int -> Tree (Tree Int)
sufijos t = sufijos' t E
    where
        sufijos' (Leaf x) sst = Leaf sst 
        sufijos' (Join lt rt) sst = let (lt',rt') = (sufijos' lt (Join rt sst)) ||| (sufijos' rt sst)
                                    in
                                    Join lt' rt'



conSufijos::Tree Int -> Tree (Int,Tree Int)
conSufijos t = conSufijos' t (sufijos t)
    where 
        conSufijos' E sss = Leaf (0,E)
        conSufijos' (Leaf x) (Leaf sst) = Leaf (x,sst)
        conSufijos' (Join lt rt) (Join slt srt) = let (lt',rt') = (conSufijos' lt slt) ||| (conSufijos' rt srt)
                                                  in
                                                  Join lt' rt'


maxT:: Tree Int -> Int
maxT E = 0
maxT (Leaf x) = x
maxT (Join lt rt) = let (lt2, rt2) = (maxT lt) ||| (maxT rt)
                    in max lt2 rt2


maxAll:: Tree (Tree Int) -> Int
maxAll (Leaf x ) = maxT x
maxAll (Join lt rt) = let (lt2,rt2) = (maxAll lt) ||| (maxAll rt)
                      in max lt2 rt2


