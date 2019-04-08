data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show

--a)
completo::a->Int-> Tree a
completo x 0 = Nodo Hoja x Hoja
completo x d = let xs = completo x (d-1)
			   in Nodo xs x xs




--b)
balanceado::a->Int->Tree a  
balanceado x 0 = Nodo Hoja x Hoja
balanceado x n = let n2 = div n 2 
                     xs = balanceado x (div n 2) 
                 in  
                     |mod n 2 == 1 = (Nodo xs x xs)
                     |otherwise = (Nodo xs x (Nodo xs x Hoja)