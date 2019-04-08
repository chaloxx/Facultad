data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving (Show,Eq)


{-
sufijos:: Tree Int -> Tree (Tree Int)
sufijos t = sufijos' t E
    where
          sufijos' E t = E
          sufijos' (Leaf x) t = Leaf t
          sufijos' (Join l r) t = let (l',r') = (sufijos' l (Join r t), sufijos' r t)
                                  in (Join l' r')-}


sufijos::Tree Int -> Tree (Tree Int)
sufijos t = sufijos' t E
    where
        sufijos' (Leaf x) sst = Leaf sst 
        sufijos' (Join lt rt) sst = let  suf = if sst == E then rt
                                               else (Join rt sst)
                                         (lt',rt') = (sufijos' lt suf,sufijos' rt sst)
                                    in Join lt' rt'


data Alumno = A {nombre::String,edad::Int} deriving Show

dato::Alumno -> String

dato (A{nombre = n,edad = e}) = n