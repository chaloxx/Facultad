module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM
 
instance Applicative StateError where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance Monad StateError where
    return x = StateError (\s -> Just (x,s))
    t >>= f  = StateError (\s -> (runStateError t s >>= \(x,s') ->
                                 (runStateError (f x) s')))

instance MonadError StateError where
    throw = StateError (\s -> Nothing)

instance MonadState StateError where
    lookfor v = StateError (\s -> lookfor' v s s)
                where lookfor' _ [] _ = Nothing
                      lookfor' v ((u, j):ss) s' | v == u = Just (j,s')
                                                | otherwise = lookfor' v ss s'
                                                      
    update v i = StateError (\s -> update' v i s)
                 where update' v i [] = Just((),[(v,i)])
                       update' v i ((a,b):xs) | v == a = Just ((),(a,i):xs)
                                              | v /= a =  update' v i xs >>=
                                                          \(x,y) -> Just ((),(a,b):y)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = case (runStateError (evalComm p) initState) of
             Nothing -> []
             Just (_,s) -> s

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var s) = lookfor s
evalIntExp (UMinus e) = do x <- evalIntExp e;
                           return (negate x)
evalIntExp (Plus e1 e2) = abstract evalIntExp (+) e1 e2
evalIntExp (Minus e1 e2) = abstract evalIntExp (-) e1 e2
evalIntExp (Times e1 e2) = abstract evalIntExp (*) e1 e2
evalIntExp (Div e1 e2) = do x <- evalIntExp e2;
                            if x == 0 then throw 
                            else abstract evalIntExp div e1 e2

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False 
evalBoolExp (Eq e1 e2) = abstract evalIntExp (==) e1 e2
evalBoolExp (Lt e1 e2) = abstract evalIntExp (<) e1 e2
evalBoolExp (Gt e1 e2) = abstract evalIntExp (>) e1 e2
evalBoolExp (And e1 e2) = abstract evalBoolExp (&&) e1 e2
evalBoolExp (Or e1 e2) = abstract evalBoolExp (||) e1 e2
evalBoolExp (Not e) = do b <- evalBoolExp e;
                         return (not b)

abstract :: Monad m => (a -> m b) -> (b -> b -> c) -> a -> a -> m c
abstract ev op e1 e2 = do x <- ev e1;
                          y <- ev e2;
                          return (op x y)
