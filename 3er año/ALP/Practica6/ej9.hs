data Error er a = Raise er
                 | Return a
                 deriving Show 


instance Functor (Error er) where
instance Applicative (Error er) where

instance Monad (Error er) where
	return x = Return x
	(Raise x) >>= f = Raise x
	(Return x) >>= f = f x




head2 :: [a] -> Error String a
head2 [] = Raise "Lista vacia"
head2 (x:xs) = Return x


tail2 :: [a] -> Error String [a]
tail2 [] = Raise "Lista vacia"
tail2 (x:xs) = Return xs
 
push :: a -> [a] -> Error String [a] 
push x ys = Return (x:ys)

pop :: [a] -> Error String a
pop [] = Raise "Lista vacia"
pop (x:xs) = Return x
 
 
newtype M s e a = M{runM :: s -> Error e (a,s)}
data T = Con Int
         | Div T T

instance Functor (M s e) where
instance Applicative (M s e) where

instance Monad (M s e) where
	return x = M (\s -> Return (x,s))
	t >>= f = M(\s -> ((runM t s) >>= \(x,y) -> runM (f x) y ))	
					  



raise ::String ->  M  Int String Int
raise s = M (\s1 -> Raise s)

modify :: (Int -> Int) -> M Int String ()
modify f = M (\s1 -> Return ((),f s1))

eval :: T -> M Int String Int
eval (Con n) = return n
eval (Div t1 t2) = (eval t1) >>= \v1 ->
                      (eval t2) >>= \v2 ->
                          if v2 == 0 then raise "Error, division por cero."
                          else modify (+1) >>= \_ -> return (div t1 t2)
							


doEval :: T -> Error String (Int, Int)
doEval t = runM (eval t) 0




