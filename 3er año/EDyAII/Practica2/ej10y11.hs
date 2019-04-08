
--Ejercicio 10:

--a) [[ ]] ++ xs = xs
--No tiene sentido


--b) [[]] ++ xs = [xs]
--No tiene sentido

--c) [[]] ++ xs = [ ] : xs
--No tiene sentido

--d) [[]] ++ xs = [[],xs]
--Tiene sentido


--e) [[]] ++ [xs] = [[],xs]
--Tiene sentido si xs es un string


--f) [[]] ++ [xs] = [xs]
--Cambiamos por lo siguiente
--[[]] ++ [xs] = [[],xs]

--g) [] ++ xs = [] : xs
--Tiene sentido si xs es una lista


--h) [] ++ xs = xs
--Tiene sentido si xs es una lista

--i)[xs] ++ [] = [xs]
--Tiene sentido 

--j) [xs ] ++ [xs ] = [xs, xs ]
--Tiene sentido

--Ejercicio 11:

modulus::[Float]->Float
modulus = sqrt . sum . map (^2)


vmod::[[Float]]->[Float]
vmod [ ]= []
vmod (v : vs) = modulus v : vmod vs
