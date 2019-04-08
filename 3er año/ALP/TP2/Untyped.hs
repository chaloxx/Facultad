module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

pos :: [String] -> String -> Int
pos (x:xs) y = if x == y then 0 else 1 + pos xs y           

conversion  :: LamTerm -> Term
conversion x = conver' [] x
    where conver' xs (Abs y l) = Lam (conver' (y:xs) l)
          conver' xs (LVar x) = if elem x xs then Bound (pos xs x)
                                else (Free x)
          conver' xs (App x y) = (conver' xs x) :@: (conver' xs y)   

  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift (Free x) _ _ = (Free x)
shift (Bound i) c d = if i >= c then Bound (i+d)  else Bound i
shift (Lam t) c d = Lam (shift t (c+1) d)
shift (t :@: t') c d = (shift t c d) :@: (shift t' c d)
  
  
subst :: Term -> Term -> Int -> Term
subst (Free x) _ _ = Free x
subst (Bound n) t' i = if n == i then t' else Bound n
subst (Lam t1) t' i = Lam (subst t1 (shift t' 0 1) (i+1))
subst (t1 :@: t2) t' i = (subst t1 t' i) :@: (subst t2 t' i)


eval :: NameEnv Term -> Term -> Term
eval xs (Free x) = let ys = [(a,b) | (a,b) <- xs, a == x]
                   in if null ys then Free x else eval xs (snd (head ys))
eval xs (Bound x)  = Bound x
eval xs (Lam t1)  = Lam (eval xs t1)
eval xs (t1:@:t2)  = case eval xs t1 of
                    (Lam t1') -> eval xs (shift (subst t1' (shift t2 0 1) 0) 0 (-1)) 
                    t1' -> let t2' = eval xs t2
                           in t1':@:t2'


