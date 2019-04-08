
data Tree a = E | Leaf a | Join (Tree a) (Tree a)
(|||)::a -> b -> (a,b)
a ||| b = (a,b)


maxtup::(Num a,Ord a) =>(a,a,a,a) -> a
maxtup (x,y,w,z) = max (max x y) (max w z)


mcss::(Num a,Ord a) => Tree a -> a
mcss t = maxtup (mcss2 t)
         where
         mcss2 E = (0,0,0,0)
         mcss2 (Leaf x) = (max x 0,max x 0,max x 0,x)
         mcss2 (Join lt rt) = let 
                     ((sml,pl,sl,suml),(smr,pr,sr,sumr)) = (mcss2 lt) ||| (mcss2 rt)
                     in
                     (max sml (max smr (sl+pr)),
                      max pl (suml+pr),
                      max sr (sumr+sl),
                      suml+sumr)



prueba = (Join (Leaf (-1)) (Leaf 1))
prueba2 = Join (Join (Leaf 3) (Leaf 2)) (Join (Leaf (-1)) (Leaf 1))
prueba3 = Join (Leaf (-1)) (Leaf 1)

prueba4 = (Join
           (Join (Join (Leaf 3) (Leaf 2)) (Join (Leaf (-1)) (Leaf 1)))
           (Join (Join (Leaf 2) (Leaf 4)) (Join (Leaf (-9)) (Leaf 8))) 
           )