{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Avl where

import Control.Monad.Par  
import GHC.Generics (Generic)
import Criterion.Measurement

{-(|||) ::NFData a => a -> a -> (a,a)
x ||| y =  runPar   $ do x' <- spawnP x  -- start evaluating (f x)
                         y' <- spawnP y  -- start evaluating (g x)
                         a  <- get x'       -- wait for fx
                         b  <- get y'        -- wait for gx
                         return (a,b)        --   return results-}

 

--main = secs <$> time_ putStrLn $ show $ insert Empty [1 .. 1000000] >>= print
-- main = putStrLn $ show $ [1 .. 10000000]

(|||) :: a -> b -> (a,b)
a ||| b = (a,b)

data AVL a = Empty | Node a (AVL a) (AVL a) deriving (Show, Generic, NFData)



{-insert :: NFData a => AVL a -> [a] -> AVL a
insert t [] = t
insert (Node m l r) zs = let (xs,ys) = split zs
                             (l',r') = (insert l xs) ||| (insert r ys) 
                         in Node m l' r'
insert Empty (x:zs) = let (xs,ys) = split zs
                          (l',r') = (insert Empty xs) ||| (insert Empty ys) 
                      in Node x l' r'                               -}

insert :: NFData a => AVL a -> a -> AVL a
insert Empty x = Node x Empty Empty
insert (Node x' l r) x = Node x' l $ rotate $ insert r x




merge :: AVL a -> AVL a -> AVL a
merge Empty t = t
merge t Empty = t
merge (Node n l1 r1) r = rotate (Node n (merge l1 r) r) 

                        
partition ::NFData a => (a -> Bool) -> AVL a -> (AVL a , AVL a )
partition p Empty = (Empty,Empty)
partition p (Node x l r) = let ((l1,l2),(r1,r2)) = (partition p l) ||| (partition p r)
                               (s1,s2)    = (merge l1 r1) ||| (merge l2 r2)                    
                            in if p x then (insert s1 x,s2)
                               else (s1, insert s2 x)
                 


                        

height :: AVL a -> Int
height Empty = -1
height (Node k l r) = let (hl,hr) = (height l) ||| (height r)
                      in 1 + (max hl hr)

                        

balanced :: AVL a -> Bool
balanced Empty = True
balanced  (Node k l r) | not (balanced l) = False
                       | not (balanced r) = False
                       | abs ((height l) - (height r)) > 1 = False
                       | otherwise = True




rotate ::  AVL a -> AVL a
rotate Empty = Empty
rotate (Node n l r) | not (balanced l) = Node n (rotate l) r
                    | not (balanced r) = Node n l (rotate r) 
                    | (height l) + 1 < (height r) &&    -- SR RR
                      (height (left r))  < (height (right r)) = 
                      Node (value r) (Node n l (left r)) (right r)                                  
                    | (height r) + 1 < (height l) &&  -- SR LL
                      (height (right l))  < (height (left l)) = 
                      Node (value l) (left l) (Node n (right l) r)
                    | (height l) + 1 < (height r) && -- DR RL
                      (height (left r))  > (height (right r)) = 
                       Node (value (left r)) (Node n l (left (left r))) 
                       (Node (value r) (right (left r)) (right r))
                    | (height r) + 1 < (height l) && -- DR LR
                      (height (right l))  > (height (left l)) = 
                      Node (value (right l)) (Node (value l) (left l) (left (right l))) 
                       (Node n (right (right l)) r)
                    | otherwise = Node n l r 


left :: AVL a -> AVL a
left Empty = Empty
left (Node n l r) = l


right :: AVL a -> AVL a
right Empty = Empty
right (Node n l r) = r
                        

-- Si el árbol es no vacio                        
value :: AVL a -> a
value (Node n l r) = n
                        








