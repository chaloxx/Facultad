
--Ejercicio 7

--1)
iff = \x -> \y -> if x then not y else y
iff2 x y = if x then not y else y

--2)
alpha = \x->x
alpha2 x = x


--Ejercicio 8
{-h x y = (f . g) x y
h x y = f(g x) y

h x = f . (g x)
h x y = (f . (g x))y = f(g(x)y) = f(g x y)

(.)::(b -> c)-> (a -> b) -> a -> c-}
--f :: c -> d
--g :: a -> b -> c
--h x y = f (g x y)
--h::a->b->d
--o tiene el sgte tipo: 
