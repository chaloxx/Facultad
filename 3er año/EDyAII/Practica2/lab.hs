module Lab01 where

import Data.List
import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
--not2::Int->Int

not2::Bool->Bool
not2 b = case b of
		 True  -> False
  		 False -> True

-- b)
in2 [x]         =  []
in2 (x:xs)      =  x :xs
in2 []          =  error "empty list"


-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- c)
length2 []        =  0
length2 (_:l)     =  1 + length2 l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
{-[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys-}



-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
listmin xs = (head . sort) xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap  f xs


--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5
five::Show a=>a->Int
five x = 5

--b) apply, que toma una función y un valor, y devuelve el resultado deaplicar la función al valor dado
f::Int->Int
f x = x
apply::(Int->Int)->Int->Int
apply f x = f x

--c) ident, la función identidad

--d) first, que toma un par ordenado, y devuelve su primera componente
first::(Int,Int)->Int
first (x,_) = x



--e) derive, que aproxima la defrivada de una función dada en un punto dado
derive::(Float->Float)->Float->Float->Float
derive f x a = (f (x+a) - f x)/(x+a - x)

--f) sign, la función signo
sign::Int->Int
sign b |b < 0 = -1 |otherwise = 1 


--g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs::Int->Int
vabs x = x * sign x

vabs2::Int->Int
vabs2 x |x < 0  = -x | otherwise = x

--h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot::Int->Int->Int
pot x y = y^x

--i) xor, el operador de disyunción exclusiva
xor::Bool->Bool->Bool
xor x y | x == y = False |otherwise = True

--j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3::Int->Int->Int->Int
max3 x y z |x >= y && x >= z = x |y >= x && y >= z = y |otherwise = z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap::(Int,Int)->(Int,Int)
swap (x,y) = (y,x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
bisiesto::Int->Bool
bisiesto anio = mod anio 4 == 0 && mod anio 100 /= 0 

{-4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}

--v = [1, 2, 3] *$ 2 *$ 4
(*$)::[Int]->Int->[Int]
[] *$ x = []
xs *$ x = map (*x) xs


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:-}

--a) 'suma', que suma todos los elementos de una lista de números
suma::[Int]->Int
suma [] = 0
suma (x:xs) = x + suma xs

{-b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario-}
alguno::[Bool]->Bool
alguno [] = False
alguno (x:xs) = if x then True else alguno xs


{-c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario-}
todos::[Bool]->Bool
todos [] = True
todos (x:xs) = x && todos xs

{-d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales-}


{-e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado-}
restos::[Int]->Int->[Int]
restos [] y = []
restos (x:xs) y = mod x y : restos xs y



{-f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados-}
cuadrados::[Int]->[Int]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs



{-g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes-}
longitudes::Show a =>[[a]]->[Int]
longitudes [] = []
longitudes (x:xs) = (length x) : longitudes xs

{-h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda-}
orden::[(Int,Int)]->[(Int,Int)]
orden [] = []
orden ((x,y):xs) = if x < (3*y) then (x,y) : orden xs
				   else orden xs



{-i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares-}
pares::[Int]->[Int]
pares [] = []
pares (x:xs) |mod x 2 == 0 = x : pares xs |otherwise = pares xs


{-j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)-}
letras::[Char]->String
letras [] = []
letras (x:xs) = if isLetter x then x : letras xs else letras xs

{-k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}
masDe::Show a => [[a]]->Int->[[a]]
masDe [] y = []
masDe (x:xs) y | length x > y = x : masDe xs y |otherwise = masDe xs y