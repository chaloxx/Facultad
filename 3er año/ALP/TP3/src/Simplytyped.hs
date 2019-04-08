module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)  = Lam t (conversion' (n:b) u)
conversion' b (LetL s t1 t2) = LetT s (conversion' b t1) (conversion' b t2)
conversion' b (AsL e t)  = AsT (conversion' b e) t 
conversion' b (UnitL) = UnitT
conversion' b (PairL t1 t2) = PairT (conversion' b t1) (conversion' b t2)
conversion' b (FstL e) = FstT (conversion' b e)
conversion' b (SndL e) = SndT (conversion' b e)
conversion' b (ZeroL) = ZeroT
conversion' b (SucL e) = SucT (conversion' b e)
conversion' b (RecL e1 e2 e3) = RecT (conversion' b e1) (conversion' b e2) (conversion' b e3)

-----------------------
--- eval
-----------------------



sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (LetT s t1 t2)        = LetT s (sub i t t1) (sub i t t2) -- Este caso no lo tenes
sub i t (AsT e t')            = AsT (sub i t e) t'
sub i t (UnitT)               = UnitT
sub i t (ZeroT)               = ZeroT
sub i t (PairT e1 e2)         = PairT (sub i t e1) (sub i t e1) 
sub i t (FstT e)              = FstT (sub i t e) -- Este tampoco
sub i t (SndT e)              = SndT (sub i t e) -- Este tampoco
sub i t (SucT t1)             = SucT (sub i t t1) 
sub i t (RecT t1 t2 t3)       = RecT (sub i t t1) (sub i t t2) (sub i t t3)



-- evaluador de términos
isValue :: Term -> Bool
isValue (Lam _ _) = True
isValue (UnitT) = True
isValue (PairT u t) = isValue u && isValue t
isValue (ZeroT) = True
isValue (SucT x) = isValue x 
isValue _ = False




eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)       = case eval e v of
                               (VLam t' u') -> eval e (Lam t u :@: Lam t' u')
                               (VUnit) -> eval e $ sub 0 UnitT u
                               (VPair t' u') -> eval e $ sub 0 (PairT (quote t') (quote u')) u
                               (VNv n) -> eval e $ sub 0 (quote $ VNv n) u                
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (LetT s t1 t2) = let v = eval e t1 
                            (Right t) = infer' [] e t1
                        in eval ((Global s,(v,t)):e) t2
eval e (AsT e' t) = if isValue e' then eval e e'
                    else let v = eval e e'
                         in eval e (AsT (quote v) t)    
eval e (UnitT) = VUnit
eval e (PairT t1 t2) = let v1 = eval e t1
                       in VPair v1 (eval e t2)                       
eval e (FstT (PairT e1 _)) = eval e e1
eval e (SndT (PairT _ e2)) = eval e e2
eval e (ZeroT) = VNv Zero
eval e (SucT e') = case eval e e' of
                   (VNv t) -> VNv (Suc t)
                   _ -> error "Error de tipo en run-time, verificar type checker"  

eval e (RecT t1 _ (ZeroT)) = eval e t1
eval e (RecT t1 t2 (SucT t3)) =  eval e (t2:@:RecT t1 t2 t3:@:t3)
eval e (RecT t1 t2 t3) = let v = eval e t3
                         in eval e (RecT t1 t2 (quote v)) 


                   
-----------------------   
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VUnit) = UnitT
quote (VPair e1 e2) = PairT (quote e1) (quote e2)
quote (VNv Zero) = ZeroT
quote (VNv (Suc t)) = SucT (quote (VNv t))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones aut1'iliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."




rError2 :: Type -> Type -> Either String Type
rError2 t1 x = err $ "R esperaba " ++
                     render (printType (Fun t1 (Fun Nat t1))) ++
                     " como segundo argumento pero " ++
                     render (printType x) ++
                     " fue inferido"

rError3 :: Type -> Either String Type
rError3 t3 = err $ "R espera Nat como tercer argumento, pero " ++
                   render (printType t3) ++
                   " fue inferido"

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1) 
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (UnitT) = ret Unit
infer' c e (LetT s t1 t2) = infer' c e t1 >>= \t ->
                            infer' c ((Global s,(VLam t t1,t)):e) t2 
infer' c e (AsT e' t) = infer' c e e' >>= \t' ->
                        if t == t' then ret t'
                        else matchError t t'
infer' c e (PairT e1 e2) =infer' c e e1 >>= \t1 ->
                          infer' c e e2 >>= \t2 ->
                          ret (PairType t1 t2)
infer' c e (FstT e1) = infer' c e e1 >>= \t ->
                       case t  of
                       PairType t1 t2 -> ret t1
                       _ -> error "Se infiere algo que no es un par"                    
infer' c e (SndT e1) = infer' c e e1 >>= \t ->
                       case t  of
                       PairType t1 t2 -> ret t2
                       _ -> error "Se infiere algo que no es un par"  

infer' c e (ZeroT) = ret Nat
infer' c e (SucT e') =  infer' c e e' >>= \t ->
                        if t == Nat then ret Nat                                              
                        else matchError Nat t 
infer' c e (RecT  e1 e2 e3) = infer' c e e1 >>= \t1 ->
                              infer' c e e3 >>= \t3 ->
                              case t3 of 
                              Nat -> infer' c e e2 >>= \t2 ->
                                     case t2 of                                    
                                     Fun t1' (Fun Nat t1'') -> if t1' == t1'' && t1' == t1 then ret t1
                                                               else notfunError t2
                                     otherwise -> matchError (Fun t1 (Fun Nat t1)) t2
                              pp -> matchError Nat pp
                                      


----------------------------------





