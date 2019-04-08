module Eval1 (eval) where

import AST
import Parser
-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Integer
lookfor v e = snd $ head $ filter ((v ==) . fst) e

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update v i e =  (v,i):filter ((v /=) . fst) e

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Let v i) e = update v (evalIntExp i e) e 
evalComm (Seq x y) e = evalComm y (evalComm x e)
evalComm (Cond x y z) e = if (evalBoolExp x e) then (evalComm y e)
                          else (evalComm z e)
evalComm  (Repeat x y) e = let e' = (evalComm x e)
                           in evalComm (Cond y Skip (Repeat x y)) e'
evalComm (While x y) e = evalComm (Cond x (Seq y (While x y)) Skip) e                          

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const n) _ = n
evalIntExp (Var n) e = lookfor n e
evalIntExp (UMinus n) e = negate (evalIntExp n e)
evalIntExp (Plus a b) e = (evalIntExp a e) + (evalIntExp b e)
evalIntExp (Minus a b) e = (evalIntExp a e) - (evalIntExp b e)
evalIntExp (Times a b) e = (evalIntExp a e) * (evalIntExp b e)
evalIntExp (Div a b) e = div (evalIntExp a e) (evalIntExp b e)



-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp (BTrue) _ = True
evalBoolExp (BFalse) _ = False
evalBoolExp (Eq a b) e  = (evalIntExp a e) == (evalIntExp b e)
evalBoolExp (Lt a b) e = (evalIntExp a e) < (evalIntExp b e)
evalBoolExp (Gt a b) e = (evalIntExp a e) > (evalIntExp b e)
evalBoolExp (And a b) e = (evalBoolExp a e) && (evalBoolExp b e)
evalBoolExp (Or a b) e = (evalBoolExp a e) || (evalBoolExp b e)
evalBoolExp (Not a) e = not (evalBoolExp a e) 