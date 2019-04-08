

sequence2 :: Monad m => [m a] -> m [a]
sequence2 [] = return []
sequence2 (x:xs) = do y <- x;
					  ys <- sequence2 xs;
					  return (y:ys)
					  
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = do y <- x;
			   return (f y)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = do x <- m1;
					y <- liftM (f x) m2;
					return y

sequence3 :: Monad m => [m a] -> m [a]
sequence3 []  = return []
sequence3 (x:xs) = do x' <- x;
			          return (f x' res) 




 




