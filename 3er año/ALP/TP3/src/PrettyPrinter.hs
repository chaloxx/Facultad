module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s

pp ii vs (i :@: c) = sep [parensIf (isLam i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (LetT s t1 t2) = text "let " <> 
                          text s <>
                          text " = " <>
                          (pp ii vs t1) <>
                          text " in " <>
                          (pp ii vs t2)                                               
pp ii vs (AsT e t) = text "(" <> (pp ii vs e) <> text ") as " <> (printType t)
pp ii vs (UnitT) = text "unit"
pp ii vs (PairT e1 e2) = text "( " <> (pp ii vs e1) <> text " , " <> (pp ii vs e2) <> text " )"
pp ii vs (FstT t) = text "fst " <> (pp ii vs t)
pp ii vs (SndT t) = text "snd " <> (pp ii vs t)  
pp ii vs (SucT e) = text "suc " <> (pp ii vs e)
pp ii vs (ZeroT) = text "0"
pp ii vs (RecT e1 e2 e3) = text "R " <>
                           (pp ii vs e1) <>
                           text " " <>
                           (pp ii vs e2) <>
                           text " " <>
                           (pp ii vs e3)


isLam :: Term -> Bool                    
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool        
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
printType (Unit)       = text "U"
printType (PairType t1 t2) = text "(" <> printType t1 <> text "," <> printType t2 <> text ")"
printType (Nat) = text "N"


isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (LetT _ t1 t2)    = (fv t1)++(fv t2)
fv (AsT e _) = fv e
fv (PairT t1 t2) =  (fv t1) ++ (fv t2) 
fv (FstT e) = fv e
fv (SndT e) = fv e
fv (RecT e1 e2 e3) = (fv e1)++(fv e2)++(fv e3)

---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

