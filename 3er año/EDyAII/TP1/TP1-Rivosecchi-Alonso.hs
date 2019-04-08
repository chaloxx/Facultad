-- Estructuras de Datos y Algoritmos II
-- Trabajo Practico I
-- Realizado por: Alonso Pablo
--                Rivosecchi Alejandro

import Data.List
 

data NdTree p = Node (NdTree p) p (NdTree p) Int
                | Empty
    deriving(Eq, Ord, Show)
    
-- ord compara la n componente de dos puntos
-- cmp devuelve True si la n componente del primer punto es mayor o
-- igual a la del segundo, sino devuelve False.
class Punto p where
    dimension::p -> Int
    coord::Int -> p -> Double
    dist::p -> p -> Double
    dist p1 p2 =  sqrt (distAux ((dimension p1)-1) p1 p2)
    ord::Int -> (p -> p -> Ordering)
    cmp::Int -> p -> (p -> Bool)
    
    

-- Ejercicio 1
-- Enunciado a

-- distAux: Calcula el cuadrado de la distancia entre p1 y p2. Su primer
-- argumento es la dimension de p1 y p2 menos uno.
distAux::Punto p => Int -> p -> p -> Double        
distAux n p1 p2 | n == 0 = ((coord n p1) - (coord n p2) )^2 
                | n > 0 = ((coord n p1) - (coord n p2) )^2 + distAux (n-1) p1 p2 
                | otherwise = error "La dimension del Punto debe ser mayor a 0" 


-- Enunciado b

newtype Punto2d = P2d(Double, Double) deriving Show
newtype Punto3d = P3d(Double,Double,Double) deriving Show
type Rect = (Punto2d,Punto2d) --Suponemos que Rect es una estructura que define la diagonal de un rectangulo

instance Punto Punto2d where
    dimension p = 2
    
    coord 0 (P2d (x,_)) = x
    coord 1 (P2d (_,y)) = y
    coord _ _ = error "La coordenada es 0 o 1"
    
    ord 0 (P2d (x,_)) (P2d (y,_)) = compare x y
    ord 1 (P2d (_,x)) (P2d (_,y)) = compare x y
    ord _ _ _ = error "Punto de dos dimensiones, se puede ordenar solo con componente 0 o 1"
    
    cmp 0 (P2d (x,_)) (P2d (y,_)) = x >= y
    cmp 1 (P2d (_,x)) (P2d (_,y)) = x >= y
    cmp _ _ _= error "Punto de dos dimensiones, solo se puede comparar la componente 0 o 1" 
    
instance Punto Punto3d where
    dimension p = 3
    
    coord 0 (P3d (x,_,_)) = x
    coord 1 (P3d (_,y,_)) = y
    coord 2 (P3d (_,_,z)) = z
    coord _ _ = error "La coordenada es 0, 1 o 2"
    
    ord 0 (P3d (x,_,_)) (P3d (y,_,_)) = compare x y
    ord 1 (P3d (_,x,_)) (P3d (_,y,_)) = compare x y
    ord 2 (P3d (_,_,x)) (P3d (_,_,y)) = compare x y
    ord _ _ _ = error "Punto de tres dimensiones, se puede ordenar solo con componente 0, 1 o 2"
    
    cmp 0 (P3d (x,_,_)) (P3d (y,_,_)) = x >= y
    cmp 1 (P3d (_,x,_)) (P3d (_,y,_)) = x >= y
    cmp 2 (P3d (_,_,x)) (P3d (_,_,y)) = x >= y
    cmp _ _ _ = error "Punto de tres dimensiones, solo se puede comparar la componente 0, 1 o 2"


instance Eq Punto2d where
    (==) (P2d(x1,y1)) (P2d(x2,y2))  = x1 == x2 && y1 == y2

instance Eq Punto3d where
    (==) (P3d(x1,y1,z1)) (P3d(x2,y2,z2))  = x1 == x2 && y1 == y2 && z1 == z2
    
-- Ejercicio 2
fromList::Punto p => [p] -> NdTree p
fromList s = hacerArbol 0 s where
    hacerArbol _ []     = Empty
    hacerArbol n (x:xs) =  let eje = mod n (dimension x)
                               ordenada = sortBy (ord eje) (x:xs)
                               posMediana = div (length ordenada) 2
                               mediana = ordenada !! posMediana
                               parteIzq = takeWhile (cmp eje mediana) ordenada -- Su ult elemento es el correspondiente al nodo que estamos creando
                               -- Es menor o igual a todos los otros elementos en parteIzq.
                               listaSubArbolIzq = init parteIzq
                               valorNodo = last parteIzq
                               listaSubArbolDer = drop ((length listaSubArbolIzq) + 1) ordenada  
                           in
                           Node (hacerArbol (n+1)  listaSubArbolIzq) valorNodo (hacerArbol (n+1) listaSubArbolDer) eje




--Ejercicio 3
insertar::Punto p =>p -> NdTree p -> NdTree p
insertar p1 t = agregarNodo 0 p1 t (dimension p1) where
    agregarNodo n p1 Empty d = Node Empty p1 Empty n
    agregarNodo n p1 (Node lt x rt e) d | cmp e x p1 = Node (agregarNodo (mod (e+1) d) p1 lt d) x rt e 
                                        | otherwise  =  Node lt x (agregarNodo (mod (e+1) d) p1 rt d) e 


--Ejercicio 4

-- minNodo devuelve el punto con el menor valor en la componente eje del NdTree pasado
minNodo::(Eq p, Punto p) => NdTree p -> Int -> p
minNodo (Node lt x rt e) eje | eje == e = if lt == Empty
                                                then x
                                                else minNodo lt eje
                             | otherwise = let minizq = if lt /= Empty then (minNodo lt eje) else x
                                               minder = if rt /= Empty then (minNodo rt eje) else x
                                               minambos = if cmp eje minizq minder
                                                              then minder
                                                              else minizq
                                           in if cmp eje x minambos
                                                  then minambos
                                                  else x
                                                
-- maxNodo devuelve el nodo con el mayor valor en la componente eje del NdTree pasado
maxNodo::(Eq p, Punto p) => NdTree p -> Int -> p
maxNodo (Node lt x rt e) eje | eje == e = if rt == Empty
                                                then x
                                                else maxNodo rt eje
                             | otherwise = let maxizq = if lt /= Empty then (maxNodo lt eje) else x
                                               maxder = if rt /= Empty then (maxNodo rt eje) else x
                                               maxambos = if cmp eje maxizq maxder
                                                              then maxizq 
                                                              else maxder 
                                           in if cmp eje x maxambos
                                                  then x 
                                                  else maxambos



 
eliminar::(Eq p,Punto p) =>p -> NdTree p -> NdTree p
eliminar p1 Empty = Empty
eliminar p1 (Node lt x rt e) | x == p1 = if rt /= Empty then let  reemplazo = minNodo rt e
                                                                  rt2 = eliminar reemplazo rt  
                                                             in Node lt reemplazo rt2 e
                                         else if lt /= Empty then let reemplazo = maxNodo lt e 
                                                                      lt2 = eliminar reemplazo lt
                                                                  in Node lt2 reemplazo rt e                         
                                         else Empty --Caso simple
                             | cmp e x p1 = Node (eliminar p1 lt) x rt e--Si  x >= p1 en el eje e buscamos en el arbol izquierdo
                             | otherwise = Node lt x (eliminar p1 rt) e--Sino en el derecho





--Ejercicio 5

ortogonalSearch:: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node lt p rt e) (p1,p2)    | x >= x1  && x <= x2 &&
                                              y >= y1  && y <= y2 = --El pto pertenece al rectangulo 
                                                
                                                --Ver para que lado del arbol hay que bajar sabiendo que el punto pertenece
                                                let eje = e
                                                    comp = coord e p
                                                    in
                                                    if comp == min (coord eje p1) (coord eje p2) then p : ortogonalSearch rt (p1,p2)
                                                    else if comp == max (coord eje p1) (coord eje p2) then p : ortogonalSearch lt (p1,p2)
                                                    else  p : (ortogonalSearch lt (p1,p2)) ++ (ortogonalSearch rt (p1,p2))
 
                                            |otherwise = --El pto no pertenece al rectangulo
                                                let eje = e
                                                    comp = coord e p
                                                    in
                                                    if  comp > max (coord eje p1) (coord eje p2) then ortogonalSearch lt (p1,p2)
                                                    else if comp < min (coord eje p1) (coord eje p2) then ortogonalSearch rt (p1,p2)
                                                    else ortogonalSearch lt (p1,p2) ++ ortogonalSearch rt (p1,p2)

                                            where x = coord 0 p
                                                  y = coord 1 p
                                                  x1 = min (coord 0 p1) (coord 0 p2)
                                                  x2 = max (coord 0 p1) (coord 0 p2)
                                                  y1 = min (coord 1 p1) (coord 1 p2)
                                                  y2 = max (coord 1 p1) (coord 1 p2)
                                                         




                                    










-- Puntos 2d
a = P2d(0,0)
b = P2d(5,7)
c = P2d(-1,-5)
d = P2d(-2,9)
e = P2d(0,6)                

ptos2d = [a,b,c,d,e]
arbol2d = (fromList ptos2d)
arbol2d2 = insertar (P2d(6,9)) arbol2d

-- Puntos 3d                           
aa = P3d(0,0,0)
bb = P3d(-1,2,-2)
cc = P3d(5,2,0)
dd = P3d(0,-2,-2)
ee = P3d(1,3,0)
ff = P3d(1,2,1)

ptos3d = [aa,bb,cc,dd,ee,ff]
arbol3d = (fromList ptos3d)


--Para correr el programa ejecutar runhaskell TP1-Rivosecchi-Alonso.hs

main = do

-- Inicio testing
    
    print (assert (fromList ([]::[Punto2d]) == Empty) "Test exitoso")
    print (assert (fromList ([]::[Punto3d]) == Empty) "Test exitoso")
    print (assert (fromList ptos2d == (Node (Node (Node Empty (P2d (-1.0,-5.0)) Empty 0) (P2d (0.0,0.0)) (Node Empty (P2d (-2.0,9.0)) Empty 0) 1) (P2d (0.0,6.0)) (Node Empty (P2d (5.0,7.0)) Empty 1) 0) ) "Test exitoso")
    print (assert (fromList ptos3d == (Node (Node (Node (Node Empty (P3d (0.0,-2.0,-2.0)) Empty 0) (P3d (0.0,0.0,0.0)) Empty 2) (P3d (-1.0,2.0,-2.0)) (Node Empty (P3d (1.0,3.0,0.0)) Empty 2) 1) (P3d (1.0,2.0,1.0)) (Node Empty (P3d (5.0,2.0,0.0)) Empty 1) 0)) "Test exitoso")
 
    print (assert (insertar (P2d(0,0)) Empty ==  (Node Empty (P2d(0,0)) Empty 0)) "Test exitoso")
    print (assert (insertar (P3d(0,0,0)) Empty ==  (Node Empty (P3d(0,0,0)) Empty 0)) "Test exitoso")
    print (assert (insertar (P2d(6,9)) arbol2d == (Node (Node (Node Empty (P2d (-1.0,-5.0)) Empty 0) (P2d (0.0,0.0)) (Node Empty (P2d (-2.0,9.0)) Empty 0) 1) (P2d (0.0,6.0)) (Node Empty (P2d (5.0,7.0)) (Node Empty (P2d (6.0,9.0)) Empty 0) 1) 0)) "Test exitoso") 
    print (assert (insertar (P3d(3,1,9)) arbol3d == (Node (Node (Node (Node Empty (P3d (0.0,-2.0,-2.0)) Empty 0) (P3d (0.0,0.0,0.0)) Empty 2) (P3d (-1.0,2.0,-2.0)) (Node Empty (P3d (1.0,3.0,0.0)) Empty 2) 1) (P3d (1.0,2.0,1.0)) (Node (Node Empty (P3d (3,1,9)) Empty 2) (P3d (5.0,2.0,0.0)) Empty 1) 0)) "Test exitoso")
    
    print (assert (eliminar (P2d(0,0)) Empty == Empty) "Test exitoso")
    print (assert (eliminar (P3d(0,0,0)) Empty == Empty) "Test exitoso")
    print (assert (eliminar (P2d(-1,-1)) arbol2d2 == arbol2d2) "Test exitoso")
    print (assert (eliminar (P2d(6,9)) arbol2d2 == (Node (Node (Node Empty (P2d (-1.0,-5.0)) Empty 0) (P2d (0.0,0.0)) (Node Empty (P2d (-2.0,9.0)) Empty 0) 1) (P2d (0.0,6.0)) (Node Empty (P2d (5.0,7.0)) Empty 1) 0)) "Test exitoso")
    print (assert (eliminar (P2d(-1,-5)) arbol2d2 == (Node (Node Empty (P2d (0.0,0.0)) (Node Empty (P2d (-2.0,9.0)) Empty 0) 1) (P2d (0.0,6.0)) (Node Empty (P2d (5.0,7.0)) (Node Empty (P2d (6.0,9.0)) Empty 0) 1) 0)) "Test exitoso")
    print (assert (eliminar (P2d(0,6)) arbol2d2 == (Node (Node (Node Empty (P2d (-1.0,-5.0)) Empty 0) (P2d (0.0,0.0)) (Node Empty (P2d (-2.0,9.0)) Empty 0) 1) (P2d (5.0,7.0)) (Node Empty (P2d (6.0,9.0)) Empty 1) 0)) "Test exitoso")
    print (assert (eliminar (P3d(-1,-1,-1)) arbol3d == arbol3d) "Test exitoso")
    print (assert (eliminar (P3d(0,-2,-2)) arbol3d == (Node (Node (Node Empty (P3d (0.0,0.0,0.0)) Empty 2) (P3d (-1.0,2.0,-2.0)) (Node Empty (P3d (1.0,3.0,0.0)) Empty 2) 1) (P3d (1.0,2.0,1.0)) (Node Empty (P3d (5.0,2.0,0.0)) Empty 1) 0) ) "Test exitoso")
    print (assert (eliminar (P3d(1,2,1)) arbol3d == (Node (Node (Node (Node Empty (P3d (0.0,-2.0,-2.0)) Empty 0) (P3d (0.0,0.0,0.0)) Empty 2) (P3d (-1.0,2.0,-2.0)) (Node Empty (P3d (1.0,3.0,0.0)) Empty 2) 1) (P3d (5.0,2.0,0.0)) Empty 0) ) "Test exitoso")
  
    print (assert (ortogonalSearch arbol2d2 (P2d(4,6),P2d(6,10)) == [P2d (5.0,7.0),P2d (6.0,9.0)]) "Test exitoso")
    print (assert (ortogonalSearch arbol2d2 (P2d(8,2),P2d(7,1)) == []) "Test exitoso")
    print (assert (ortogonalSearch arbol2d2 (P2d(4,6),P2d(5.5,7.2)) == [P2d (5.0,7.0)]) "Test exitoso")
    print (assert (ortogonalSearch arbol2d2 (P2d(-4,-6),P2d(10,20)) == [P2d (0.0,6.0),P2d (0.0,0.0),P2d (-1.0,-5.0),P2d (-2.0,9.0),P2d (5.0,7.0),P2d (6.0,9.0)]) "Test exitoso")

-- Fin testing
