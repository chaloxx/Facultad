
--1
f1a x = let (y, z ) = (x , x ) in y

f1b x = x


--2
greater (x , y) = if x > y then True else False

greater2(x,y) = x > y

--3
g(a,b) = a - b

f3a (x , y) = let z = x + y in g (z , y) where g (a, b) = a - b

f3b(x,y) = g (x + y, y)