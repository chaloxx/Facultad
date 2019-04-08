data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show


maxBST::Ord a =>BST a -> a
maxBST Hoja = error "No se puede"
maxBST (Nodo l n Hoja) = n
maxBST (Nodo l n r) = maxBST r

minBST::Ord a => BST a -> a
minBST Hoja = error "No se puede"
minBST (Nodo Hoja n r) = n
minBST (Nodo l n r) = minBST l


checkBST::Ord a =>BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja n Hoja) = True
checkBST (Nodo (Nodo xl x xr) n Hoja) = checkBST (Nodo xl x xr) && x <= n
checkBST (Nodo Hoja n (Nodo yl y yr)) = checkBST (Nodo yl y yr) && y > n
checkBST (Nodo (Nodo xl x xr) n (Nodo yl y yr))=   checkBST (Nodo xl x xr) &&
                                                   checkBST (Nodo yl y yr) &&
                                                    x <= n && y > n




member::Ord a =>a->BST a ->Bool
member x Hoja = False
member x (Nodo l n r)  | x <= n = memberAux x l n
                       | otherwise = member x r

memberAux::Int->BST a->Int->Bool
memberAux x Empty y = x == y
memberAux x (Nodo l n r) y | x <= n = memberAux x l n
                           | otherwise = memberAux x r y





