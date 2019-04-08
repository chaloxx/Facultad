module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f  = State (\s -> let (v, s') = runState m s
                            in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
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
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var s) = lookfor s
evalIntExp (UMinus e) = do x <- evalIntExp e;
                           return (negate x)
evalIntExp (Plus e1 e2) = abstract evalIntExp (+) e1 e2
evalIntExp (Minus e1 e2) = abstract evalIntExp (-) e1 e2
evalIntExp (Times e1 e2) = abstract evalIntExp (*) e1 e2
evalIntExp (Div e1 e2) = abstract evalIntExp div e1 e2

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
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
