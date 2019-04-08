



instance Applicative (State s) where
instance Functor (State s) where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show



instance Functor Tree where
	fmap f (Leaf x) = Leaf (f x)
	fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
	


	
mapTreeNro :: (a -> Int -> (b,Int)) -> Tree a -> Int -> (Tree b , Int)
mapTreeNro f (Leaf x) n = let (n1,n2) = f x n
						  in (Leaf n1,n2)
						  
mapTreeNro f (Branch l r) n = let (l',n1) = mapTreeNro f l n
							  in let (r',n2) = mapTreeNro f r n1
							     in  (Branch l' r', n2)
							      
							      
mapTreeSt :: (a -> s -> (b,s)) -> Tree a -> s -> (Tree b, s)
mapTreeSt f (Leaf x) n = let (n1,n2) = f x n
	   			         in (Leaf n1,n2)
mapTreeSt f (Branch l r) n = let (l',n1) = mapTreeSt f l n
							  in let (r',n2) = mapTreeSt f r n1
							     in  (Branch l' r', n2) 							      
			
			
			
			
			
			
newtype State s a = St {runState :: s -> (a,s)}
instance Monad (State s) where 
	return x = St (\s -> (x,s))
	(St h) >>= f = St (\s -> let (x,s') = h s 
							 in runState (f x) s' )			
			
			
							       
mapTreeM :: (a -> State s b) -> Tree a -> State s (Tree b)
mapTreeM f (Leaf x) = f x >>= \t -> return (Leaf t)  
mapTreeM f (Branch l r) = do x <- mapTreeM f l;
							 y <- mapTreeM f r;
							 return (Branch x y)
							      
							      
							      
							      
estadoNulo :: Int
estadoNulo = 1
							      
numTree :: Tree a -> Tree Int
numTree t = let x = runState (mapTreeM update t)
			in fst (x estadoNulo)

valor = mapTreeM update (Leaf "h")
							      
update a = St (\s -> (s,s+1))
							      
									
							      
