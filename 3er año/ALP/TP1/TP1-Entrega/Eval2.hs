module Eval2 (eval) where

import AST
import Parser
-- Estados
type State = [(Variable,Integer)]

data Error = DivByZero | UndefVar deriving (Eq,Show)

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor v e = let e' = (filter ((v ==) . fst) e)
              in if (null e') then Left UndefVar
                 else Right (snd (head e'))
  

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> State -> Integer -> State
update v e i = (v,i):(filter ((v /=) . fst) e)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Either Error State
evalComm Skip e = Right e
evalComm  (Let v i) e = abstract (Right . (update v e)) evalIntExp i e
evalComm (Seq x y) e = abstract (evalComm y) evalComm x e
evalComm (Cond x y z) e = let f a = if a then (evalComm y e) else (evalComm z e)                          							  
                          in abstract f evalBoolExp x e 						  
evalComm  (Repeat x y) e = abstract (evalComm (Cond y Skip (Repeat x y))) evalComm x e 						    
evalComm  (While x y) e = evalComm (Cond x (Seq y (While x y)) Skip) e
 

abstract right eval x e = let x' = eval x e
                          in either Left right x' 




-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const n) _ = Right n
evalIntExp (Var n) e = lookfor n e
evalIntExp (UMinus n) e = either Left (Right . negate) (evalIntExp n e)
evalIntExp (Plus a b) e = checkerror (+) (evalIntExp a e) (evalIntExp b e)
evalIntExp (Minus a b) e = checkerror (-) (evalIntExp a e) (evalIntExp b e)
evalIntExp (Times a b) e = checkerror (*) (evalIntExp a e) (evalIntExp b e)
evalIntExp (Div a b) e = let b' = (evalIntExp b e)
                        in case b' of
                              (Right 0) -> Left DivByZero
                              otherwise -> checkerror div (evalIntExp a e) b'
                           



-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp (BTrue) _ = Right True
evalBoolExp (BFalse) _ = Right False
evalBoolExp (Eq a b) e  = checkerror (==) (evalIntExp a e) (evalIntExp b e)
evalBoolExp (Lt a b) e  = checkerror (<) (evalIntExp a e) (evalIntExp b e)
evalBoolExp (Gt a b) e  = checkerror (>) (evalIntExp a e) (evalIntExp b e)
evalBoolExp (And a b) e = checkerror (&&) (evalBoolExp a e) (evalBoolExp b e)
evalBoolExp (Or a b) e  = checkerror (||) (evalBoolExp a e) (evalBoolExp b e)
evalBoolExp (Not a) e  = either Left (Right . not) (evalBoolExp a e)

checkerror op e1 e2 = case e1 of
                                  (Left x) -> Left x
                                  (Right x) -> case e2 of
                                               (Left y) -> Left y
                                               (Right y) -> Right (op x y)