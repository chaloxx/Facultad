




newtype Output w a = Out (a,w)

instance Functor (Output w)  where
instance Applicative (Output w)  where


instance Monoid w => Monad (Output w) where
	return x = Out(x,mempty)
	Out(x,y) >>= f = let Out(x',y') = f x
					 in Out(x',mappend y y')

			 
	
write :: Monoid w => w -> Output w () 	
write w = Out((),w)
					 


data Exp a = Con a 
		   | Plus (Exp a) (Exp a)
		   | Div (Exp a) (Exp a)
		   deriving Show 


  
	
consMsje:: Show a => Exp a -> a -> String
consMsje t n = "El termino " ++ "(" ++ (show t) ++ ")" ++ " tiene valor " ++ (show n) ++ " \n "
	
eval ::  Exp Int -> Output String Int
eval t@(Con n) = do write (consMsje t n);
                    return n					
eval t@(Div e1 e2) = do x <- eval e1;
						y <- eval e2;
						write (consMsje t (div x y));
					    return (div x y)
eval t@(Plus e1 e2) = do x <- eval e1;
						 y <- eval e2;
						 let n = x+y
						 in do write (consMsje t n);
						       return n
												



evalMain :: Exp Int -> IO ()
evalMain t = let Out(x,y) = eval t
             in putStr (y ++ "Resultado: " ++ show x ++ "\n")
             
             
             
