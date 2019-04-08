
zip3a::[a]->[a]->[a]->[(a,a,a)]
zip3a [] _ _ = []
zip3a _ [] _ = []
zip3a _ _ [] = []
zip3a (x:xs) (y:ys) (z:zs) = (x,y,z): zip3a xs ys zs
 
--zip3b xs ys zs = [(x,y,z) | ((x,y),z) <- zip (zip xs ys) zs]


zip3b::[a]->[a]->[a]->[(a,a,a)]
zip3b xs ys zs = map (\((x,y),z)->(x,y,z)) (zip (zip xs ys) zs)






length2:: [a] -> Int
length2 [] = 0
length2 (x : xs) = 1 + length2 xs
