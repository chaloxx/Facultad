


smallest (x , y, z ) | x <= y && x <= z = x
					 | y <= x && y <= z = y
					 | z <= x && z <= y = z


smallest2 = \x y z-> | x <= y && x <= z = x| y <= x && y <= z = y| z <= x && z <= y = z))


--2)
--second x = λx → x
second = \x->x

--3)
--andThen, definida por
--andThen True y = y
--andThen False y = False
andThen = \x y -> if x then y else False



--4)
f x = x+1
twice f x = f (f x )
twice2 = \x->(f (f x))

--5)
f2 x y = x-y 
flip f2 x y = f2 y x 
flip2 = \x->(\y->(\f2->f2 y x))


--6)
inc x = (+1)x
inc2 = \x-> x + 1
