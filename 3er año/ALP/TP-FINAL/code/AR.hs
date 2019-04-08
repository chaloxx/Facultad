module AR where


import Data.HashMap.Strict hiding (foldr,map)
import Avl
import AST
import DmlFunctions (obtainUpd)
import Url

exProd :: Env -> [String] -> Either String  (AVL (HashMap String Args))
exProd _ [] = Right E
exProd e (s:ls) =  do v <- obtainUpd (url' e s) s
                      case v of 
                        Nothing -> return (Left ("La tabla " ++ s ++ " no existe"))
                        (Just t) -> case  exProd r ls of
                                 Left m -> Left m
                                 Right t' -> Right (prod t t')



prod :: AVL (HashMap String Args) -> AVL (HashMap String Args) -> AVL (HashMap String Args)
prod E t = E
prod t E = E
prod t1 t2 = let (tl,tr) = prod (left t1) t2 ||| prod (right t1) t2
                 t = merge tl tr
             in merge t (prod' (value t1) t2)
  where prod' x t = mapT (\y -> union x y) t

