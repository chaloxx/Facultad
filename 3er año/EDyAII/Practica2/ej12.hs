type NumBin = [Bool]


sumbin::NumBin->NumBin->NumBin
sumbin [] xs = xs
sumbin xs [] = xs
sumbin (x:xs) (y:ys) = if x && y 
					   then [False] ++ sumbin [True] (sumbin xs ys)
					   else [x || y] ++ sumbin xs ys



prodbin::NumBin->NumBin->NumBin
prodbin [] xs = []
prodbin xs [] = []
prodbin (x:xs) ys = sumbin (map (&&x) ys ) ([False] ++ (prodbin xs ys))



mod2::NumBin->NumBin
mod2 (x:xs) = [x]


--cocientediv2::NumBin->NumBin
cocientediv2 (x:xs)|not x = xs |otherwise = [] 