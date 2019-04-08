data T a = E | N (T a) a (T a) deriving Show

(|||)::a -> b -> (a,b)
a ||| b = (a,b)

altura:: T a -> Integer
altura E = 0
altura (N l x r ) = 1 + (max (altura l) (altura r ))


obtenerHoja::T a -> a
obtenerHoja (N E x rt) = x
obtenerHoja (N lt x rt) = obtenerHoja lt

eliminarHoja::T a -> T a
eliminarHoja (N E x E) = E
eliminarHoja (N lt x rt) = N (eliminarHoja lt) x rt 

combinar::T a -> T a -> T a
combinar E t2 = t2
combinar t1 t2 = let  x = obtenerHoja t1
                      t1' = eliminarHoja t1
                 in N t1' x t2




filterT::(Integer -> Bool) -> T Integer -> T Integer
filterT f E = E
filterT f (N lt x rt) | f x = N lt' x rt'
                      |otherwise = combinar lt' rt'
                      where (lt',rt') = (filterT f lt) ||| (filterT f rt)


quicksort::T Integer -> T Integer
quicksort E = E
quicksort (N lt x rt) = let mayorX = \z-> (z > x)
                            menorX = \z-> (z <= x)
                            (t1,t2) = (filterT mayorX  lt) ||| (filterT menorX lt)
                            (t1',t2') = (filterT mayorX  rt) ||| (filterT menorX rt)
                            mayoresX = combinar t1 t1' 
                            menoresX = combinar t2 t2'
                        in N (quicksort menoresX) x (quicksort mayoresX)


prueba1 = N (N E 2 E) 3 (N E 1 E)
prueba2 = N E 3 (N E 2 E)
prueba3 = N (N (N E 11 E) 14 (N E 1 E)) 9 (N (N E 2 E) 4 (N E 3 E))

flattenT::T a -> [a]     
flattenT E = []
flattenT (N lt x rt) = (flattenT lt) ++ [x] ++ (flattenT rt)


