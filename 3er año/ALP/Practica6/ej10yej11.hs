---10

data Cont r a = Cont ((a -> r) -> r)


instance Functor (Cont r) where
instance Applicative (Cont r) where



instance Monad (Cont r) where
    return x = Cont (\f -> f x)
    Cont f >>= g = Cont (\a -> f (\r -> let Cont h = g r
                                        in h a))



---11





data M m a = Mk (m (Maybe a))

instance Monad m => Functor (M m) where
instance Monad m => Applicative (M m) where

instance Monad m => Monad (M m) where
    return x = Mk (return (Just x))
    (Mk t) >>= f = Mk(t >>= \x -> case x of
                                    Nothing -> return Nothing 
                                    (Just w) -> let Mk t' = f w
                                                in t')


throw :: Monad m =>  M m a
throw = Mk (return Nothing)

data StInt a = St (Int -> (a, Int))
newtype N a = Mr (StInt a)

instance Functor N where
instance Applicative N where


instance Monad N  where
	return x = Mr (St (\s -> (x,s)))
	Mr (St t) >>= f = Mr (St (\s -> let (x,s') = t s
	                                    Mr (St t') = f x 
                                    in  t' s'))


get :: N Int
get = Mr (St (\x -> (x,x)))


put ::  Int -> N ()
put n = Mr (St (\s -> ((),n)))

data Exp  = Var 
            | Con Int
            | Let Exp Exp
            | Add Exp Exp
            | Div Exp Exp
            
            
eval :: Exp -> N Int 
eval (Var) = get
eval (Con n) = return n
eval (Add t1 t2) =  do x <- eval t1;
                       y <- eval t2;
                       return (x+y)
eval (Div t1 t2) =  do x <- eval t1;
                       y <- eval t2;
                       return (div x y)
eval (Let t1 t2) = do v <- eval t1;
                      put v;
                      eval t2                                             
            
            
evalMain :: Exp -> Int           
evalMain e = let Mr (St t) = eval e
             in fst(t 0)



