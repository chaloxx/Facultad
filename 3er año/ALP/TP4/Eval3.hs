module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

newtype StateErrorTick a = StateErrorT { runStateError :: Env -> Maybe (a, Env, Int) }

instance Monad StateErrorTick where
  return x = StateErrorT (\s -> Just (x,s,0))
  t >>= f  = StateErrorT (\s -> (runStateError t s >>= \(x,s',n) ->
                                let Just (a,b,c) = (runStateError (f x) s')
                                in Just (a,b,c+n)))

class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a
    
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()
        
class Monad m => MonadTick m where
  tick :: m ()        

instance MonadError StateErrorTick where
    throw = StateErrorT (\s -> Nothing)
  
instance MonadTick StateErrorTick where  
  tick = StateErrorT (\s -> Just ((),s,1))
    
-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM
 
instance Applicative StateErrorTick where
    pure   = return
    (<*>)  = ap    

instance MonadState StateErrorTick where
       lookfor v = StateErrorT (\s -> lookfor' v s s)
                    where lookfor' _ [] _ = Nothing
                          lookfor' v ((u, j):ss) s' | v == u = Just (j,s',0)
                                                    | otherwise = lookfor' v ss s'
                                                      
       update v i = StateErrorT (\s -> update' v i s)
                     where update' v i [] = Just((),[(v,i)], 0)
                           update' v i ((a,b):xs) | v == a = Just ((), (a,i):xs, 0)
                                                  | v /= a =  update' v i xs >>= \(x,y,n) -> Just ((),(a,b):y, n)
-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Evalua un programa en el estado nulo
eval :: Comm -> (Env,Int)
eval p = case (runStateError (evalComm p) initState) of
         Nothing -> ([],0)
         Just (_,s,n) -> (s,n)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm (Skip) = return ()
evalComm (Let v e) = do  x <- evalIntExp e;
                         update v x
evalComm (Seq com1 com2) = (evalComm com1) >> (evalComm com2)
evalComm (Cond expB com1 com2) = do b <- evalBoolExp expB;
                                    if b then evalComm com1
                                    else evalComm com2
evalComm (While expB com) = do b <- evalBoolExp expB;
                               if b then evalComm (Seq com (While expB com))
                               else  (evalComm Skip)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var s) = lookfor s
evalIntExp (UMinus e) = do x <- evalIntExp e;
                           return (negate x)
evalIntExp (Plus e1 e2) = abstract evalIntExp (+) e1 e2 True
evalIntExp (Minus e1 e2) = abstract evalIntExp (-) e1 e2 True
evalIntExp (Times e1 e2) = abstract evalIntExp (*) e1 e2 True
evalIntExp (Div e1 e2) = do x <- evalIntExp e2;
                            if x == 0 then throw 
                            else abstract evalIntExp div e1 e2 True
              
-- Evalua una expresion entera, sin efectos laterales
evalBoolExp ::(MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False 
evalBoolExp (Eq e1 e2) = abstract evalIntExp (==) e1 e2 False
evalBoolExp (Lt e1 e2) = abstract evalIntExp (<) e1 e2 False
evalBoolExp (Gt e1 e2) = abstract evalIntExp (>) e1 e2 False
evalBoolExp (And e1 e2) = abstract evalBoolExp (&&) e1 e2 False
evalBoolExp (Or e1 e2) = abstract evalBoolExp (||) e1 e2 False
evalBoolExp (Not e) = do b <- evalBoolExp e;
                         return (not b)

abstract :: MonadTick m => (a -> m b) -> (b -> b -> c) -> a -> a -> Bool -> m c
abstract ev op e1 e2 b = do x <- ev e1;
                            y <- ev e2;
                            if b then do tick; 
                                         return (op x y)
                            else return (op x y)
