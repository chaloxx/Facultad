{-# LANGUAGE DeriveFoldable , MagicHash #-}


module Avl where
import COrdering
import Data.HashMap.Strict hiding (foldr,map,size,null,join,delete)
import GHC.Exts
import Prelude hiding (lookup)
import Data.Maybe (isJust,fromJust)
import Data.Int
import Data.Bits((.&.),shiftR)

-- | A BinPath is full if the search succeeded, empty otherwise.
data BinPath a = FullBP   Int a -- Found
               | EmptyBP  Int   -- Not Found


(|||) :: a -> b -> (a,b)
a ||| b = (a,b)


data AVL e = E                      -- ^ Empty Tree
           | N (AVL e) e (AVL e)    -- ^ BF=-1 (right height > left height)
           | Z (AVL e) e (AVL e)    -- ^ BF= 0
           | P (AVL e) e (AVL e)    -- ^ BF=+1 (left height > right height)
           deriving(Eq,Ord,Show,Read,Foldable)







emptyT :: AVL e
emptyT = E







filterT :: (e -> Bool ) -> AVL e -> AVL e
filterT f E = E
filterT f t = let (bl,br) = filterT f (left t) ||| filterT f (right t)
                  r    = join bl br
              in case f (value t) of
                    True -> pushL (value t) r
                    False -> r


-- Dado un avl ordenado y uno  desordenado, mueve todos los elementos de t2
-- a t1 devolviendo un árbol ordenado
mergeT :: (e -> e -> COrdering e) -> AVL e -> AVL e -> AVL e
mergeT f t E = t
mergeT f t t' = let t1 = mergeT f t (left t')
                    t2 = mergeT f t1 (right t')
               in push (f $ value t') (value t') t2





foldT :: (a -> b -> b -> b) -> b -> AVL a -> b
foldT _ b E = b
foldT f b t = let (l,r) = foldT f b (left t) ||| foldT f b (right t)
              in f (value t) l r

listToTree :: [e] -> AVL e
listToTree [] = E
listToTree (x:xs) = pushL x  (listToTree xs)


isEmpty :: AVL e -> Bool
isEmpty E = True
isEmpty _ = False


singletonT :: e -> AVL e
singletonT e = Z E e E

isSingletonT :: AVL e -> Bool
isSingletonT (Z E _ E) = True
isSingletonT _ = False



height :: AVL e -> Int
height t = addHeight 0 t

-- | Adds the height of a tree to the first argument.
--
-- Complexity: O(log n)
addHeight :: Int -> AVL e -> Int
addHeight h  E        = h
addHeight h (N l _ _) = addHeight (h + 2) l
addHeight h (Z l _ _) = addHeight (h + 1) l
addHeight h (P _ _ r) = addHeight (h + 2) r



right :: AVL e -> AVL e
right (N l k r) = r
right (P l k r) = r
right (Z l k r) = r

left :: AVL e -> AVL e
left (N l k r) = l
left (P l k r) = l
left (Z l k r) = l

value :: AVL e -> e
value (N _ k _) = k
value (P _ k _) = k
value (Z _ k _) = k





particionT :: (a -> Bool) -> AVL a -> (AVL a , AVL a)
particionT p E = (E,E)
particionT p t = let ((l1,l2),(r1,r2)) = particionT p (left t) ||| particionT p (right t)
                     (t1,t2) = join l1 r1 ||| join l2 r2
                 in case p (value t) of
                      True ->  (t1, pushL (value t) t2)
                      False -> (pushL (value t) t1,t2)




particionT2 :: (a -> Either c c) -> (a -> b) -> AVL a -> (AVL c,AVL b)
particionT2 p f E = (E,E)
particionT2 p f t = let ((l1,l2),(r1,r2)) = (particionT2 p f (left t)) ||| (particionT2 p f (right t))
                        (l,r) = (join l1 r1) ||| (join l2 r2)
                        x = value t
                    in case p x of
                        Right msg -> ( l, pushL (f x) r)
                        Left msg -> (pushL msg l,r)


toTree :: [a] -> AVL a
toTree [] = E
toTree (x:xs) = pushL x (toTree xs)


toSortedTree :: Ord a => [a] -> AVL a
toSortedTree [] = E
toSortedTree (x:xs) = push (sndCC x) x (toSortedTree xs)



{-sortedT :: (e -> e -> COrdering e) -> AVL e -> AVL e
sortedT f t = sortedT' f t E
 where sortedT' f E t = t
       sortedT' f t t' = let t1 = sortedT' f (left t) t'
                             t2 = sortedT' f (right t) t1
                         in push (f (value t)) (value t) t2-}

sortedT :: (e -> e -> COrdering e) -> AVL e -> AVL e
sortedT _  E = E
sortedT f t = let (l,r) = sortedT f (left t) ||| sortedT f (right t)
                  t' = unionT f l r
              in push (f (value t)) (value t) t'





{---------------  mapT  ----------------------------------}

mapT :: (e -> b) ->  AVL e -> AVL b
mapT _ E = E
mapT f t = let (l',r') = mapT f (left t) ||| mapT f (right t)
           in case t of
                (N _ e _) -> N l' (f e) r'
                (Z _ e _) -> Z l' (f e) r'
                (P _ e _) -> P l' (f e) r'














-- Complexity: O(log n)
pushL :: e -> AVL e -> AVL e
pushL e0 = pushL' where  -- There now follows a cut down version of the more general put.
                         -- Insertion is always on the left subtree.
                         -- Re-Balancing cases RR,RL/LR(1/2) never occur. Only LL!
                         -- There are also more impossible cases (putZL never returns N)
 ----------------------------- LEVEL 0 ---------------------------------
 --                             pushL'                                --
 -----------------------------------------------------------------------
 pushL'  E        = Z E e0 E
 pushL' (N l e r) = putNL l e r
 pushL' (Z l e r) = putZL l e r
 pushL' (P l e r) = putPL l e r

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 putNL  E           e r = Z (Z E e0 E) e r            -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          P _ _ _ -> Z l' e r         -- L subtree BF:0->+1, H:h->h+1, parent BF:-1-> 0
                          _       -> error "pushL: Bug0" -- impossible

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 putZL  E           e r = P (Z E e0 E) e r            -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> error "pushL: Bug1" -- impossible
                          _       -> P l' e r         -- L subtree BF: 0->+1, H:h->h+1, parent BF: 0->+1

      -------- This case (PL) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 putPL  E           _ _ = error "pushL: Bug2"         -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = putPLL ll le lr e r         -- LL (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putPLL                                 --
 -----------------------------------------------------------------------

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 putPLL  E le lr e r              = Z (Z E e0 E) le (Z lr e r)          -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putNL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putPL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZL lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> error "pushL: Bug3" -- impossible
                                    _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+1, H:h->h+1, parent BF:-1->-2, CASE LL !!
-----------------------------------------------------------------------
--------------------------- pushL Ends Here ---------------------------
-----------------------------------------------------------------------


-- Complexity: O(log n)
pushR :: AVL e -> e -> AVL e
pushR t e0 = pushR' t where  -- There now follows a cut down version of the more general put.
                             -- Insertion is always on the right subtree.
                             -- Re-Balancing cases LL,RL/LR(1/2) never occur. Only RR!
                             -- There are also more impossible cases (putZR never returns P)

 ----------------------------- LEVEL 0 ---------------------------------
 --                             pushR'                                --
 -----------------------------------------------------------------------
 pushR'  E        = Z E e0 E
 pushR' (N l e r) = putNR l e r
 pushR' (Z l e r) = putZR l e r
 pushR' (P l e r) = putPR l e r

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 putZR l e E            = N l e (Z E e0 E)            -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> N l e r'         -- R subtree BF: 0->-1, H:h->h+1, parent BF: 0->-1
                          _       -> error "pushR: Bug0" -- impossible

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 putPR l e  E           = Z l e (Z E e0 E)            -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          N _ _ _ -> Z l e r'         -- R subtree BF:0->-1, H:h->h+1, parent BF:+1-> 0
                          _       -> error "pushR: Bug1" -- impossible

      -------- This case (NR) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 putNR _ _ E            = error "pushR: Bug2"         -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = putNRR l e rl re rr         -- RR (never returns P)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putNRR                                 --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re (Z E e0 E)          -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putNR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putPR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZR rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    Z _ _ _ -> N l e (Z rl re rr')      -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> Z (Z l e rl) re rr'      -- RR subtree BF: 0->-1, H:h->h+1, parent BF:-1->-2, CASE RR !!
                                    _       -> error "pushR: Bug3"      -- impossible
-----------------------------------------------------------------------
--------------------------- pushR Ends Here ---------------------------
-----------------------------------------------------------------------





-- Complexity: O(log n)
push :: (e -> COrdering e) -> e -> AVL e -> AVL e
push c e0 = put where -- there now follows a huge collection of functions requiring
                         -- pattern matching from hell in which c and e0 are free variables
-- This may look longwinded, it's been done this way to..
--  * Avoid doing case analysis on the same node more than once.
--  * Minimise heap burn rate (by avoiding explicit rebalancing operations).
 ----------------------------- LEVEL 0 ---------------------------------
 --                              put                                  --
 -----------------------------------------------------------------------
 put  E        = Z    E e0 E
 put (N l e r) = putN l e  r
 put (Z l e r) = putZ l e  r
 put (P l e r) = putP l e  r

 ----------------------------- LEVEL 1 ---------------------------------
 --                       putN, putZ, putP                            --
 -----------------------------------------------------------------------

 -- Put in (N l e r), BF=-1  , (never returns P)
 putN l e r = case c e of
              Lt    -> putNL l e  r  -- <e, so put in L subtree
              Eq e' -> N     l e' r  -- =e, so update existing
              Gt    -> putNR l e  r  -- >e, so put in R subtree

 -- Put in (Z l e r), BF= 0
 putZ l e r = case c e of
              Lt    -> putZL l e  r  -- <e, so put in L subtree
              Eq e' -> Z     l e' r  -- =e, so update existing
              Gt    -> putZR l e  r  -- >e, so put in R subtree

 -- Put in (P l e r), BF=+1 , (never returns N)
 putP l e r = case c e of
              Lt    -> putPL l e  r  -- <e, so put in L subtree
              Eq e' -> P     l e' r  -- =e, so update existing
              Gt    -> putPR l e  r  -- >e, so put in R subtree

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 {-# INLINE putNL #-}
 putNL  E           e r = Z (Z    E  e0 E ) e r       -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push: Bug0" -- impossible
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          _       -> Z l' e r         -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 {-# INLINE putZL #-}
 putZL  E           e r = P (Z    E  e0 E ) e r       -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push: Bug1" -- impossible
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> P l' e r         -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 {-# INLINE putZR #-}
 putZR l e E            = N l e (Z    E  e0 E )       -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push: Bug2" -- impossible
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> N l e r'         -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 {-# INLINE putPR #-}
 putPR l e  E           = Z l e (Z    E  e0 E )       -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push: Bug3" -- impossible
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          _       -> Z l e r'         -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

      -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 {-# INLINE putNR #-}
 putNR _ _ E            = error "push: Bug4"               -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putN rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putP rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = case c re of                        -- determine if RR or RL
                          Lt     -> putNRL l e    rl re  rr   -- RL (never returns P)
                          Eq re' ->    N   l e (Z rl re' rr)  -- new re
                          Gt     -> putNRR l e    rl re  rr   -- RR (never returns P)

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 {-# INLINE putPL #-}
 putPL  E           _ _ = error "push: Bug5"               -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putN ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putP ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = case c le of                        -- determine if LL or LR
                          Lt     -> putPLL  ll le  lr  e r    -- LL (never returns N)
                          Eq le' ->    P (Z ll le' lr) e r    -- new le
                          Gt     -> putPLR  ll le  lr  e r    -- LR (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                        putNRR, putPLL                             --
 --                        putNRL, putPLR                             --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re (Z E e0 E)         -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putN rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putP rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZ rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    E       -> error "push: Bug6"   -- impossible
                                    Z _ _ _ -> N l e (Z rl re rr')     -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z (Z l e rl) re rr'     -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = Z (Z E e0 E) le (Z lr e r)         -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putN lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putP lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZ lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    E       -> error "push: Bug7"   -- impossible
                                    Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

 -- (putNRL l e rl re rr): Put in RL subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRL #-}
 putNRL l e  E              re rr = Z (Z l e E) e0 (Z E re rr)         -- l and rr must also be E, special CASE LR !!
 putNRL l e (N rll rle rlr) re rr = let rl' = putN rll rle rlr         -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (P rll rle rlr) re rr = let rl' = putP rll rle rlr         -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (Z rll rle rlr) re rr = let rl' = putZ rll rle rlr         -- RL subtree BF= 0, so need to look for changes
                                    in case rl' of
                                    E                -> error "push: Bug8" -- impossible
                                    Z _    _    _    -> N l e (Z rl' re rr)                -- RL subtree BF: 0-> 0, H:h->h, so no change
                                    N rll' rle' rlr' -> Z (P l e rll') rle' (Z rlr' re rr) -- RL subtree BF: 0->-1, SO.. CASE R1 !!
                                    P rll' rle' rlr' -> Z (Z l e rll') rle' (N rlr' re rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

 -- (putPLR ll le lr e r): Put in LR subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLR #-}
 putPLR ll le  E              e r = Z (Z ll le E) e0 (Z E e r)         -- r and ll must also be E, special CASE LR !!
 putPLR ll le (N lrl lre lrr) e r = let lr' = putN lrl lre lrr         -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (P lrl lre lrr) e r = let lr' = putP lrl lre lrr         -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (Z lrl lre lrr) e r = let lr' = putZ lrl lre lrr         -- LR subtree BF= 0, so need to look for changes
                                    in case lr' of
                                    E                -> error "push: Bug9" -- impossible
                                    Z _    _    _    -> P (Z ll le lr') e r                -- LR subtree BF: 0-> 0, H:h->h, so no change
                                    N lrl' lre' lrr' -> Z (P ll le lrl') lre' (Z lrr' e r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                    P lrl' lre' lrr' -> Z (Z ll le lrl') lre' (N lrr' e r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
------------------------- push Ends Here ----------------------------
-----------------------------------------------------------------------



{----------------------  join  -------------------------------------}
-- Complexity: O(log n), where n is the size of the larger of the two trees.
join :: AVL e -> AVL e -> AVL e
join l r = joinH' l (height l) r (height r)

joinH':: AVL e -> Int -> AVL e -> Int -> AVL e
joinH' l hl r hr = if hl <= hr then let d = hr - hl in joinHL d l r
                   else let d = hl - hr in joinHR d l r

-- hr >= hl, join l to left subtree of r.
-- Int argument is absolute difference in tree height, hr-hl (>=0)
joinHL :: Int -> AVL e -> AVL e -> AVL e
joinHL _  E           r = r                                                  -- l was empty
joinHL d (N ll le lr) r = case popRN ll le lr of
                          (l_,e) -> case l_ of
                                        E       -> error "joinHL: Bug0"       -- impossible if BF=-1
                                        Z _ _ _ -> spliceL l_ e (d + 1) r     -- hl2=hl-1
                                        _       -> spliceL l_ e  d  r         -- hl2=hl
joinHL d (Z ll le lr) r = case popRZ ll le lr of
                          (l_,e) -> case l_ of
                                        E       -> e `pushL` r               -- l had only one element
                                        _       -> spliceL l_ e d  r         -- hl2=hl
joinHL d (P ll le lr) r = case popRP ll le lr of
                          (l_,e) -> case l_ of
                                        E       -> error "joinHL: Bug1"      -- impossible if BF=+1
                                        Z _ _ _ -> spliceL l_ e (d + 1) r    -- hl2=hl-1
                                        _       -> spliceL l_ e  d  r        -- hl2=hl



-- hl >= hr, join r to right subtree of l.
-- Int argument is absolute difference in tree height, hl-hr (>=0)
joinHR :: Int -> AVL e -> AVL e -> AVL e
joinHR _ l  E           = l                                    -- r was empty
joinHR d l (N rl re rr) = case popLN rl re rr of
                          (e,r_) -> case r_ of
                                        E       -> error "joinHR: Bug0"      -- impossible if BF=-1
                                        Z _ _ _ -> spliceR r_ e (d + 1) l -- hr2=hr-1
                                        _       -> spliceR r_ e  d  l -- hr2=hr
joinHR d l (Z rl re rr) = case popLZ rl re rr of
                          (e,r_) -> case r_ of
                                        E       -> l `pushR` e            -- r had only one element
                                        _       -> spliceR r_ e d l       -- hr2=hr
joinHR d l (P rl re rr) = case popLP rl re rr of
                          (e,r_) -> case r_ of
                                        E       -> error "joinHL: Bug1"      -- impossible if BF=+1
                                        Z _ _ _ -> spliceR r_ e (d + 1) l -- hr2=hr-1
                                        _       -> spliceR r_ e  d  l -- hr2=hr
-----------------------------------------------------------------------
--------------------------- joinH' Ends Here --------------------------
-----------------------------------------------------------------------




-- hr >= hl, splice s to left subtree of r, using b as the bridge
-- The Int argument is the absolute difference in tree height, hr-hl (>=0)
spliceL :: AVL e -> e -> Int -> AVL e -> AVL e
spliceL s b 0 r           = Z s b r
spliceL s b 1 r           = N s b r
spliceL s b d   (N rl re rr) = spliceLN s b (d - 2) rl re rr   -- height diff of rl is two less
spliceL s b d   (Z rl re rr) = spliceLZ s b (d - 1) rl re rr   -- height diff of rl is one less
spliceL s b d   (P rl re rr) = spliceLP s b (d - 1) rl re rr   -- height diff of rl is one less
spliceL _ _ _    E           = error "spliceL: Bug0"              -- r can't be empty

-- Splice into left subtree of (N l e r), height cannot change as a result of this
spliceLN :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceLN s b 0 l e r = Z (Z s b l) e r                                             -- dH=0
spliceLN s b 1 l  e r = Z (N s b l) e r                                             -- dH=0
spliceLN s b d  (N ll le lr) e r = let l_ = spliceLN s b  (d - 2) ll le lr in l_ `seq` N l_ e r
spliceLN s b d   (Z ll le lr) e r = let l_ = spliceLZ s b (d - 1) ll le lr
                                    in case l_ of
                                       Z _ _ _ -> N l_ e r                                      -- dH=0
                                       P _ _ _ -> Z l_ e r                                      -- dH=0
                                       _       -> error "spliceLN: Bug0"                        -- impossible
spliceLN s b d   (P ll le lr) e r = let l_ = spliceLP s b (d - 1) ll le lr in l_ `seq` N l_ e r
spliceLN _ _ _    E           _ _ = error "spliceLN: Bug1"                                      -- impossible

-- Splice into left subtree of (Z l e r), Z->P if dH=1, Z->Z if dH=0
spliceLZ :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceLZ s b 1 l           e r = P (N s b l) e r                                                -- Z->P, dH=1
spliceLZ s b d   (N ll le lr) e r = let l_ = spliceLN s b (d - 2) ll le lr in l_ `seq` Z l_ e r -- Z->Z, dH=0
spliceLZ s b d   (Z ll le lr) e r = let l_ = spliceLZ s b (d - 1) ll le lr
                                    in case l_ of
                                       Z _ _ _ -> Z l_ e r                                      -- Z->Z, dH=0
                                       P _ _ _ -> P l_ e r                                      -- Z->P, dH=1
                                       _       -> error "spliceLZ: Bug0"                        -- impossible
spliceLZ s b d   (P ll le lr) e r = let l_ = spliceLP s b (d - 1) ll le lr in l_ `seq` Z l_ e r -- Z->Z, dH=0
spliceLZ _ _ _    E           _ _ = error "spliceLZ: Bug1"                                      -- impossible

-- Splice into left subtree of (P l e r), height cannot change as a result of this
spliceLP :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceLP s b 1 (N ll le lr) e r = Z (P s b ll) le (Z lr e r)                                     -- dH=0
spliceLP s b 1 (Z ll le lr) e r = Z (Z s b ll) le (Z lr e r)                                     -- dH=0
spliceLP s b 1 (P ll le lr) e r = Z (Z s b ll) le (N lr e r)                                     -- dH=0
spliceLP s b d    (N ll le lr) e r = let l_ = spliceLN s b (d - 2) ll le lr in l_ `seq` P l_ e r -- dH=0
spliceLP s b d    (Z ll le lr) e r = spliceLPZ s b (d - 1) ll le lr e r                          -- dH=0
spliceLP s b d    (P ll le lr) e r = let l_ = spliceLP s b (d - 1) ll le lr in l_ `seq` P l_ e r -- dH=0
spliceLP _ _ _     E           _ _ = error "spliceLP: Bug0"

-- Splice into left subtree of (P (Z ll le lr) e r)
{-# INLINE spliceLPZ #-}
spliceLPZ :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> e -> AVL e -> AVL e
spliceLPZ s b 1 ll             le lr e r = Z (N s b ll) le (Z lr e r)                        -- dH=0
spliceLPZ s b d   (N lll lle llr) le lr e r = let ll_ = spliceLN s b (d - 2) lll lle llr     -- dH=0
                                              in  ll_ `seq` P (Z ll_ le lr) e r
spliceLPZ s b d   (Z lll lle llr) le lr e r = let ll_ = spliceLZ s b (d - 1) lll lle llr     -- dH=0
                                              in case ll_ of
                                                 Z _ _ _ -> P (Z ll_ le lr) e r                 -- dH=0
                                                 P _ _ _ -> Z ll_ le (Z lr e r)                 -- dH=0
                                                 _       -> error "spliceLPZ: Bug0"             -- impossible
spliceLPZ s b d   (P lll lle llr) le lr e r = let ll_ = spliceLP s b (d - 1) lll lle llr     -- dH=0
                                              in  ll_ `seq` P (Z ll_ le lr) e r
spliceLPZ _ _ _    E              _  _  _ _ = error "spliceLPZ: Bug1"
-----------------------------------------------------------------------
-------------------------- spliceL Ends Here --------------------------
-----------------------------------------------------------------------





-- hl >= hr, splice s to right subtree of l, using b as the bridge
-- The Int argument is the absolute difference in tree height, hl-hr (>=0)
spliceR :: AVL e -> e -> Int -> AVL e -> AVL e
spliceR s b 0 l           = Z l b s
spliceR s b 1 l           = P l b s
spliceR s b d   (N ll le lr) = spliceRN s b (d - 1) ll le lr   -- height diff of lr is one less
spliceR s b d   (Z ll le lr) = spliceRZ s b (d - 1) ll le lr   -- height diff of lr is one less
spliceR s b d   (P ll le lr) = spliceRP s b (d - 2) ll le lr   -- height diff of lr is two less
spliceR _ _ _    E           = error "spliceR: Bug0"              -- l can't be empty

-- Splice into right subtree of (P l e r), height cannot change as a result of this
spliceRP :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceRP s b 0 l e  r           = Z l e (Z r b s)                                             -- dH=0
spliceRP s b 1 l e  r           = Z l e (P r b s)                                             -- dH=0
spliceRP s b d    l e (N rl re rr) = let r_ = spliceRN s b (d - 1) rl re rr in r_ `seq` P l e r_
spliceRP s b d    l e (Z rl re rr) = let r_ = spliceRZ s b (d - 1) rl re rr
                                     in case r_ of
                                        Z _ _ _ -> P l e r_                                      -- dH=0
                                        N _ _ _ -> Z l e r_                                      -- dH=0
                                        _       -> error "spliceRP: Bug0"                        -- impossible
spliceRP s b d    l e (P rl re rr) = let r_ = spliceRP s b (d - 2) rl re rr in r_ `seq` P l e r_
spliceRP _ _ _    _ _  E           = error "spliceRP: Bug1"                                      -- impossible

-- Splice into right subtree of (Z l e r), Z->N if dH=1, Z->Z if dH=0
spliceRZ :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceRZ s b 1 l e  r           = N l e (P r b s)                                                -- Z->N, dH=1
spliceRZ s b d    l e (N rl re rr) = let r_ = spliceRN s b (d - 1) rl re rr in r_ `seq` Z l e r_ -- Z->Z, dH=0
spliceRZ s b d    l e (Z rl re rr) = let r_ = spliceRZ s b (d - 1) rl re rr
                                     in case r_ of
                                        Z _ _ _ -> Z l e r_                                         -- Z->Z, dH=0
                                        N _ _ _ -> N l e r_                                         -- Z->N, dH=1
                                        _       -> error "spliceRZ: Bug0"                           -- impossible
spliceRZ s b d    l e (P rl re rr) = let r_ = spliceRP s b (d - 2) rl re rr in r_ `seq` Z l e r_ -- Z->Z, dH=0
spliceRZ _ _ _    _ _  E           = error "spliceRZ: Bug1"                                         -- impossible

-- Splice into right subtree of (N l e r), height cannot change as a result of this
spliceRN :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> AVL e
spliceRN s b 1 l e (N rl re rr) = Z (P l e rl) re (Z rr b s)                                     -- dH=0
spliceRN s b 1 l e (Z rl re rr) = Z (Z l e rl) re (Z rr b s)                                     -- dH=0
spliceRN s b 1 l e (P rl re rr) = Z (Z l e rl) re (N rr b s)                                     -- dH=0
spliceRN s b d    l e (N rl re rr) = let r_ = spliceRN s b (d - 1) rl re rr in r_ `seq` N l e r_ -- dH=0
spliceRN s b d    l e (Z rl re rr) = spliceRNZ s b (d - 1) l e rl re rr                          -- dH=0
spliceRN s b d    l e (P rl re rr) = let r_ = spliceRP s b (d - 1) rl re rr in r_ `seq` N l e r_ -- dH=0
spliceRN _ _ _    _ _  E           = error "spliceRN: Bug0"

-- Splice into right subtree of (N l e (Z rl re rr))
spliceRNZ :: AVL e -> e -> Int -> AVL e -> e -> AVL e -> e -> AVL e -> AVL e
spliceRNZ s b 1 l e rl re rr              = Z (Z l e rl) re (P rr b s)                        -- dH=0
spliceRNZ s b d    l e rl re (N rrl rre rrr) = let rr_ = spliceRN s b (d - 1) rrl rre rrr
                                               in  rr_ `seq` N l e (Z rl re rr_)                 -- dH=0
spliceRNZ s b d    l e rl re (Z rrl rre rrr) = let rr_ = spliceRZ s b (d - 1) rrl rre rrr     -- dH=0
                                               in case rr_ of
                                                  Z _ _ _ -> N l e (Z rl re rr_)                 -- dH=0
                                                  N _ _ _ -> Z (Z l e rl) re rr_                 -- dH=0
                                                  _       -> error "spliceRNZ: Bug0"             -- impossible
spliceRNZ s b d    l e rl re (P rrl rre rrr) = let rr_ = spliceRP s b (d - 2) rrl rre rrr     -- dH=0
                                               in rr_ `seq` N l e (Z rl re rr_)
spliceRNZ _ _ _    _ _ _  _   E              = error "spliceRNZ: Bug1"
-----------------------------------------------------------------------
-------------------------- spliceR Ends Here --------------------------
-----------------------------------------------------------------------








-- Compara 2 registros que contengan los atributos k
c :: Ord v => [String] -> HashMap String v -> HashMap String v -> COrdering (HashMap String v)
c [] _ y = Eq y
c (k:ks) x y =  let (v1,v2) =  (lookup k x,lookup k y) in
                  if isJust v1  && isJust v2  then case (fromJust v1) `compare` (fromJust v2) of
                                                  LT -> Lt
                                                  GT -> Gt
                                                  EQ -> c ks x y
                  else error "Error fatal"


c2 :: Ord v => [String] -> HashMap String v -> HashMap String v -> Ordering
c2 [] _ _ = EQ
c2 (k:ks) x y  = let (v1,v2) =  (lookup k x,lookup k y) in
                 if isJust v1  && isJust v2  then case (fromJust v1) `compare` (fromJust v2) of
                                                  LT -> LT
                                                  GT -> GT
                                                  EQ -> c2 ks x y
                 else error "Error fatal"





m :: Ord v => [String] -> AVL (HashMap String v) -> AVL (HashMap String v) -> AVL (HashMap String v)
m k = mergeT (c k)



ioEitherMapT :: (e -> IO(Either a b)) -> AVL e -> IO(Either a (AVL b))
ioEitherMapT f E = return $ Right E
ioEitherMapT f t = do l' <- ioEitherMapT f (left t)
                      r' <- ioEitherMapT f (right t)
                      v'   <- f $ value t
                      return $ do l'' <- l'
                                  r'' <- r'
                                  v <- v'
                                  case t of
                                   (N _ _ _) -> Right $ N l'' v r''
                                   (Z _ _ _) -> Right $ Z l'' v r''
                                   (P _ _ _) -> Right $ P l'' v r''


ioEitherFilterT :: (e -> IO(Either a Bool)) -> AVL e -> IO(Either a (AVL e))
ioEitherFilterT _ E = return $ Right E
ioEitherFilterT f t = do l' <- ioEitherFilterT f (left t)
                         r' <- ioEitherFilterT f (right t)
                         b' <- f (value t)
                         return $ do
                         l <- l'
                         r <- r'
                         b <- b'
                         let t' = join l r
                         if b then Right $ pushL (value t) t'
                         else Right t'


-- Dados los atributos k y un x definido sobre los atributos k, chequea si x pertenece a t
isMember :: Ord v => [String] -> HashMap String v -> AVL(HashMap String v) -> Bool
isMember k _ E = False
isMember k x t = case c k x  (value t) of
                     Eq _  -> True
                     Lt  -> isMember k x (left t)
                     Gt  -> isMember k x (right t)





-- Hace una busqueda según los atributos k y los valores v y tal vez devuelve un valor v
search :: Ord v => [String] -> HashMap String v  -> AVL(HashMap String v) -> Maybe (HashMap String v)
search k _ E = Nothing
search k x t = case c k x (value t) of
                 Eq v -> return v
                 Lt -> search k x (left t)
                 Gt -> search k x (right t)






 -- Complexity: O(log n)
deleteT :: (e -> Ordering) -> AVL e -> Maybe (AVL e)
deleteT c t = case findFullPath c t of
              (-1) -> Nothing                -- Not found, p<0
              p     -> Just $ deletePath p t   -- Found, so delete



-- Complexity: O(log n)
findFullPath :: (e -> Ordering) -> AVL e -> Int
findFullPath c t = find 1 0 t where
 find  _ _  E        = -1
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       LT    -> let d_ = d+d in find d_  (i+d) l
                       EQ    -> i
                       GT    -> let d_ = d+d in find d_  (i+d_) r -- d_ = 2d


deletePath :: Int -> AVL e -> AVL e
deletePath _ E         = error "deletePath: Element not found."
deletePath p (N l e r) = delN p l e r
deletePath p (Z l e r) = delZ p l e r
deletePath p (P l e r) = delP p l e r

---------------------------- LEVEL 1 ---------------------------------
--                       delN, delZ, delP                            --
-----------------------------------------------------------------------

-- Delete from (N l e r)
delN :: Int -> AVL e -> e -> AVL e -> AVL e
delN p l e r = case sel p of
               LT -> delNL p l e r
               EQ -> subN l r
               GT -> delNR p l e r

-- Delete from (Z l e r)
delZ :: Int -> AVL e -> e -> AVL e -> AVL e
delZ p l e r = case sel p of
               LT -> delZL p l e r
               EQ -> subZR l r
               GT -> delZR p l e r

-- Delete from (P l e r)
delP :: Int -> AVL e -> e -> AVL e -> AVL e
delP p l e r = case sel p of
               LT -> delPL p l e r
               EQ -> subP l r
               GT -> delPR p l e r

----------------------------- LEVEL 2 ---------------------------------
--                      delNL, delZL, delPL                          --
--                      delNR, delZR, delPR                          --
-----------------------------------------------------------------------

-- Delete from the left subtree of (N l e r)
delNL :: Int -> AVL e -> e -> AVL e -> AVL e
delNL p t = dNL (goL p) t
{-# INLINE dNL #-}
dNL :: Int -> AVL e -> e -> AVL e -> AVL e
dNL _  E           _ _ = error "deletePath: Element not found."              -- Left sub-tree is empty
dNL p (N ll le lr) e r = case sel p of
                         LT -> chkLN  (delNL p ll le lr) e r
                         EQ -> chkLN  (subN  ll    lr) e r
                         GT -> chkLN  (delNR p ll le lr) e r
dNL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` N l' e r  -- height can't change
                         EQ -> chkLN' (subZR ll    lr) e r                    -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` N l' e r  -- height can't change
dNL p (P ll le lr) e r = case sel p of
                         LT -> chkLN  (delPL p ll le lr) e r
                         EQ -> chkLN  (subP  ll    lr) e r
                         GT -> chkLN  (delPR p ll le lr) e r

-- Delete from the right subtree of (N l e r)
delNR :: Int -> AVL e -> e -> AVL e -> AVL e
delNR p t = dNR (goR p) t
{-# INLINE dNR #-}
dNR :: Int -> AVL e -> e -> AVL e -> AVL e
dNR _ _ _  E           = error "delNR: Bug0"             -- Impossible
dNR p l e (N rl re rr) = case sel p of
                         LT -> chkRN  l e (delNL p rl re rr)
                         EQ -> chkRN  l e (subN  rl    rr)
                         GT -> chkRN  l e (delNR p rl re rr)
dNR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` N l e r'   -- height can't change
                         EQ -> chkRN' l e (subZL rl    rr)                    -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` N l e r'   -- height can't change
dNR p l e (P rl re rr) = case sel p of
                         LT -> chkRN  l e (delPL p rl re rr)
                         EQ -> chkRN  l e (subP  rl    rr)
                         GT -> chkRN  l e (delPR p rl re rr)

-- Delete from the left subtree of (Z l e r)
delZL :: Int -> AVL e -> e -> AVL e -> AVL e
delZL p t = dZL (goL p) t
{-# INLINE dZL #-}
dZL :: Int -> AVL e -> e -> AVL e -> AVL e
dZL _  E           _ _ = error "deletePath: Element not found."               -- Left sub-tree is empty
dZL p (N ll le lr) e r = case sel p of
                         LT -> chkLZ  (delNL p ll le lr) e r
                         EQ -> chkLZ  (subN  ll    lr) e r
                         GT -> chkLZ  (delNR p ll le lr) e r
dZL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` Z l' e r  -- height can't change
                         EQ -> chkLZ'  (subZR ll    lr) e r                  -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` Z l' e r  -- height can't change
dZL p (P ll le lr) e r = case sel p of
                         LT -> chkLZ  (delPL p ll le lr) e r
                         EQ -> chkLZ  (subP  ll    lr) e r
                         GT -> chkLZ  (delPR p ll le lr) e r

-- Delete from the right subtree of (Z l e r)
delZR :: Int -> AVL e -> e -> AVL e -> AVL e
delZR p t = dZR (goR p) t
{-# INLINE dZR #-}
dZR :: Int -> AVL e -> e -> AVL e -> AVL e
dZR _ _ _  E           = error "deletePath: Element not found."              -- Right sub-tree is empty
dZR p l e (N rl re rr) = case sel p of
                         LT -> chkRZ  l e (delNL p rl re rr)
                         EQ -> chkRZ  l e (subN  rl    rr)
                         GT -> chkRZ  l e (delNR p rl re rr)
dZR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` Z l e r'  -- height can't change
                         EQ -> chkRZ' l e (subZL rl rr)                      -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` Z l e r'  -- height can't change
dZR p l e (P rl re rr) = case sel p of
                         LT -> chkRZ  l e (delPL p rl re rr)
                         EQ -> chkRZ  l e (subP    rl    rr)
                         GT -> chkRZ  l e (delPR p rl re rr)

-- Delete from the left subtree of (P l e r)
-- aca entra con1 , goL se transforma en 2
delPL :: Int -> AVL e -> e -> AVL e -> AVL e
delPL p t = dPL (goL p) t
{-# INLINE dPL #-}
dPL :: Int -> AVL e -> e -> AVL e -> AVL e
dPL _  E           _ _ = error "delPL: Bug0"             -- Impossible
dPL p (N ll le lr) e r = case sel p of
                         LT -> chkLP  (delNL p ll le lr) e r
                         EQ -> chkLP  (subN    ll    lr) e r
                         GT -> chkLP  (delNR p ll le lr) e r
dPL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` P l' e r  -- height can't change
                         EQ -> chkLP' (subZR ll lr) e r                        -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` P l' e r  -- height can't change-}

dPL p (P ll le lr) e r = case sel p of
                         LT -> chkLP  (delPL p ll le lr) e r
                         EQ -> chkLP  (subP    ll    lr) e r
                         GT -> chkLP  (delPR p ll le lr) e r

-- Delete from the right subtree of (P l e r)
delPR :: Int -> AVL e -> e -> AVL e -> AVL e
delPR p t = dPR (goR p) t
{-# INLINE dPR #-}
dPR :: Int -> AVL e -> e -> AVL e -> AVL e
dPR _ _ _  E           = error "deletePath: Element not found."               -- Right sub-tree is empty
dPR p l e (N rl re rr) = case sel p of
                         LT -> chkRP  l e (delNL p rl re rr)
                         EQ -> chkRP  l e (subN    rl    rr)
                         GT -> chkRP  l e (delNR p rl re rr)
dPR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` P l e r'  -- height can't change
                         EQ -> chkRP' l e (subZL rl rr)                        -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` P l e r'  -- height can't change
dPR p l e (P rl re rr) = case sel p of
                         LT -> chkRP  l e (delPL p rl re rr)
                         EQ -> chkRP  l e (subP    rl    rr)
                         GT -> chkRP  l e (delPR p rl re rr)
-----------------------------------------------------------------------
----------------------- deletePath Ends Here --------------------------
-----------------------------------------------------------------------




sel :: Int -> Ordering
sel p = if p == 0 then EQ
                    else if bit0 p then LT -- Left  if Bit 0 == 1
                                   else GT -- Right if Bit 0 == 0

bit0 p = (p .&. 1) == 1


-- Substitute deleted element from (N l _ r)
subN :: AVL e -> AVL e -> AVL e
subN _  E            = error "subN: Bug0"      -- Impossible
subN l (N rl re rr)  = case popLN rl re rr of (e,r_) -> chkRN  l e r_
subN l (Z rl re rr)  = case popLZ rl re rr of (e,r_) -> chkRN' l e r_
subN l (P rl re rr)  = case popLP rl re rr of (e,r_) -> chkRN  l e r_


-- Substitute deleted element from (Z l _ r)
-- Pops the replacement from the right sub-tree, so result may be (P _ _ _)
subZR :: AVL e -> AVL e -> AVL e
subZR _  E            = E   -- Both left and right subtrees must have been empty
subZR l (N rl re rr)  = case popLN rl re rr of (e,r_) -> chkRZ  l e r_
subZR l (Z rl re rr)  = case popLZ rl re rr of (e,r_) -> chkRZ' l e r_
subZR l (P rl re rr)  = case popLP rl re rr of (e,r_) -> chkRZ  l e r_

-- Substitute deleted element from (P l _ r)
subP :: AVL e -> AVL e -> AVL e
subP  E           _  = error "subP: Bug0"      -- Impossible
subP (N ll le lr) r  = case popRN ll le lr of (l_,e) -> chkLP  l_ e r
subP (Z ll le lr) r  = case popRZ ll le lr of (l_,e) -> chkLP' l_ e r
subP (P ll le lr) r  = case popRP ll le lr of (l_,e) -> chkLP  l_ e r


goL :: Int -> Int
goL p = shiftR p 1


goR :: Int -> Int
goR p = shiftR (p-1) 1




-- Check for height changes in left subtree of (N l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLN :: AVL e -> e -> AVL e -> AVL e
chkLN l e r = case l of
              E       -> error "chkLN: Bug0"   -- impossible if BF<>0
              N _ _ _ -> N l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> rebalN l e r          -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> N l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (Z l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLZ :: AVL e -> e -> AVL e -> AVL e
chkLZ l e r = case l of
              E       -> error "chkLZ: Bug0"   -- impossible if BF<>0
              N _ _ _ -> Z l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> N l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> Z l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (P l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLP :: AVL e -> e -> AVL e -> AVL e
chkLP l e r = case l of
              E       -> error "chkLP: Bug0"   -- impossible if BF<>0
              N _ _ _ -> P l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> Z l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> P l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (N l e r),
-- where r was (N rl re rr) or (P rl re rr)
chkRN :: AVL e -> e -> AVL e -> AVL e
chkRN l e r = case r of
              E       -> error "chkRN: Bug0"   -- impossible if BF<>0
              N _ _ _ -> N l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> Z l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> N l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (Z l e r),
-- where r was (N rl re rr) or (P rl re rr)
chkRZ :: AVL e -> e -> AVL e -> AVL e
chkRZ l e r = case r of
              E       -> error "chkRZ: Bug0"   -- impossible if BF<>0
              N _ _ _ -> Z l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> P l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> Z l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (P l e r),
-- where l was (N rl re rr) or (P rl re rr)
chkRP :: AVL e -> e -> AVL e -> AVL e
chkRP l e r = case r of
              E       -> error "chkRP: Bug0"   -- impossible if BF<>0
              N _ _ _ -> P l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> rebalP l e r          -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> P l e r               -- BF +/-1 -> +1, so dH= 0



-- Check for height changes in left subtree of (N l e r),
-- where l was (Z ll le lr)
chkLN' :: AVL e -> e -> AVL e -> AVL e
chkLN' l e r = case l of
               E       -> rebalN l e r  -- BF 0 -> E, so dH=-1
               _       -> N l e r       -- Otherwise dH=0
-- Check for height changes in left subtree of (Z l e r),
-- where l was (Z ll le lr)
chkLZ' :: AVL e -> e -> AVL e -> AVL e
chkLZ' l e r = case l of
               E       -> N l e r      -- BF 0 -> E, so dH=-1
               _       -> Z l e r      -- Otherwise dH=0
-- Check for height changes in left subtree of (P l e r),
-- where l was (Z ll le lr)
chkLP' :: AVL e -> e -> AVL e -> AVL e
chkLP' l e r = case l of
               E       -> Z l e r      -- BF 0 -> E, so dH=-1
               _       -> P l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (N l e r),
-- where r was (Z rl re rr)
chkRN' :: AVL e -> e -> AVL e -> AVL e
chkRN' l e r = case r of
               E       -> Z l e r      -- BF 0 -> E, so dH=-1
               _       -> N l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (Z l e r),
-- where r was (Z rl re rr)
chkRZ' :: AVL e -> e -> AVL e -> AVL e
chkRZ' l e r = case r of
               E       -> P l e r      -- BF 0 -> E, so dH=-1
               _       -> Z l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (P l e r),
-- where l was (Z rl re rr)
chkRP' :: AVL e -> e -> AVL e -> AVL e
chkRP' l e r = case r of
               E       -> rebalP l e r -- BF 0 -> E, so dH=-1
               _       -> P l e r      -- Otherwise dH=0


-- Local utility to substitute deleted element from (Z l _ r)
-- Pops the replacement from the left sub-tree, so result may be (N _ _ _)
subZL :: AVL e -> AVL e -> AVL e
subZL  E           _  = E   -- Both left and right subtrees must have been empty
subZL (N ll le lr) r  = case popRN ll le lr of (l_,e) -> chkLZ  l_ e r
subZL (Z ll le lr) r  = case popRZ ll le lr of (l_,e) -> chkLZ' l_ e r
subZL (P ll le lr) r  = case popRP ll le lr of (l_,e) -> chkLZ  l_ e r



-----------------------------------------------------------------------
------------------------ popL Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popL LEVEL 1 -------------------------------
--                      popLN, popLZ, popLP                          --
-----------------------------------------------------------------------
-- Delete leftmost from (N l e r)
popLN :: AVL e -> e -> AVL e -> (e,AVL e)
popLN  E           e r = (e,r)                  -- Terminal case, r must be of form (Z E re E)
popLN (N ll le lr) e r = case popLN ll le lr of
                         (v,l) -> let t = chkLN l e r in  t `seq` (v,t)
popLN (Z ll le lr) e r = popLNZ ll le lr e r
popLN (P ll le lr) e r = case popLP ll le lr of
                         (v,l) -> let t = chkLN l e r in  t `seq` (v,t)

-- Delete leftmost from (Z l e r)
popLZ :: AVL e -> e -> AVL e -> (e,AVL e)
popLZ  E           e _ = (e,E)                  -- Terminal case, r must be E
popLZ (N ll le lr) e r = popLZN ll le lr e r
popLZ (Z ll le lr) e r = popLZZ ll le lr e r
popLZ (P ll le lr) e r = popLZP ll le lr e r

-- Delete leftmost from (P l e r)
popLP :: AVL e -> e -> AVL e -> (e,AVL e)
popLP  E           _ _ = error "popLP: Bug!"        -- Impossible if BF=+1
popLP (N ll le lr) e r = case popLN ll le lr of
                         (v,l) -> let t = chkLP l e r in  t `seq` (v,t)
popLP (Z ll le lr) e r = popLPZ ll le lr e r
popLP (P ll le lr) e r = case popLP ll le lr of
                         (v,l) -> let t = chkLP l e r in  t `seq` (v,t)

-------------------------- popL LEVEL 2 -------------------------------
--                     popLNZ, popLZZ, popLPZ                        --
--                        popLZN, popLZP                             --
-----------------------------------------------------------------------

-- Delete leftmost from (N (Z ll le lr) e r), height of left sub-tree can't change in this case
popLNZ :: AVL e -> e -> AVL e -> e -> AVL e -> (e,AVL e)
{-# INLINE popLNZ #-}
popLNZ  E              le _  e r = let t = rebalN E e r              -- Terminal case, Needs rebalancing
                                   in  t `seq` (le,t)
popLNZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   (v,l) -> (v, N l e r)
popLNZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   (v,l) -> (v, N l e r)
popLNZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   (v,l) -> (v, N l e r)

-- Delete leftmost from (Z (Z ll le lr) e r), height of left sub-tree can't change in this case
-- Don't INLINE this!
popLZZ :: AVL e -> e -> AVL e -> e -> AVL e -> (e,AVL e)
popLZZ  E              le _  e r = (le, N E e r)                     -- Terminal case
popLZZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   (v,l) -> (v, Z l e r)
popLZZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   (v,l) -> (v, Z l e r)
popLZZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   (v,l) -> (v, Z l e r)

-- Delete leftmost from (P (Z ll le lr) e r), height of left sub-tree can't change in this case
popLPZ :: AVL e -> e -> AVL e -> e -> AVL e -> (e,AVL e)
{-# INLINE popLPZ #-}
popLPZ  E              le _  e _ = (le, Z E e E)                     -- Terminal case
popLPZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   (v,l) -> (v, P l e r)
popLPZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   (v,l) -> (v, P l e r)
popLPZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   (v,l) -> (v, P l e r)

-- Delete leftmost from (Z (N ll le lr) e r)
-- Don't INLINE this!
popLZN :: AVL e -> e -> AVL e -> e -> AVL e -> (e,AVL e)
popLZN ll le lr e r = case popLN ll le lr of
                      (v,l) -> let t = chkLZ l e r in  t `seq` (v,t)
-- Delete leftmost from (Z (P ll le lr) e r)
-- Don't INLINE this!
popLZP :: AVL e -> e -> AVL e -> e -> AVL e -> (e,AVL e)
popLZP ll le lr e r = case popLP ll le lr of
                      (v,l) -> let t = chkLZ l e r in t `seq` (v,t)
-----------------------------------------------------------------------
-------------------------- popL Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
------------------------ popR Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popR LEVEL 1 -------------------------------
--                      popRN, popRZ, popRP                          --
-----------------------------------------------------------------------
-- Delete rightmost from (N l e r)
popRN :: AVL e -> e -> AVL e -> (AVL e,e)
popRN _ _  E           = error "popRN: Bug!"        -- Impossible if BF=-1
popRN l e (N rl re rr) = case popRN rl re rr of
                          (r,v) -> let t = chkRN l e r in t `seq` (t,v)
popRN l e (Z rl re rr) = popRNZ l e rl re rr
popRN l e (P rl re rr) = case popRP rl re rr of
                          (r,v) -> let t = chkRN l e r in t `seq` (t,v)

-- Delete rightmost from (Z l e r)
popRZ :: AVL e -> e -> AVL e -> (AVL e,e)
popRZ _ e  E           = (E,e)                  -- Terminal case, l must be E
popRZ l e (N rl re rr) = popRZN l e rl re rr
popRZ l e (Z rl re rr) = popRZZ l e rl re rr
popRZ l e (P rl re rr) = popRZP l e rl re rr

-- Delete rightmost from (P l e r)
popRP :: AVL e -> e -> AVL e -> (AVL e,e)
popRP l e  E           = (l,e)                  -- Terminal case, l must be of form (Z E le E)
popRP l e (N rl re rr) = case popRN rl re rr of
                         (r,v) -> let t = chkRP l e r in t `seq` (t,v)
popRP l e (Z rl re rr) = popRPZ l e rl re rr
popRP l e (P rl re rr) = case popRP rl re rr of
                         (r,v) -> let t = chkRP l e r in t `seq` (t,v)

-------------------------- popR LEVEL 2 -------------------------------
--                     popRNZ, popRZZ, popRPZ                        --
--                        popRZN, popRZP                             --
-----------------------------------------------------------------------

-- Delete rightmost from (N l e (Z rl re rr)), height of right sub-tree can't change in this case
popRNZ :: AVL e -> e -> AVL e -> e -> AVL e -> (AVL e,e)
{-# INLINE popRNZ #-}
popRNZ _ e _  re  E              = (Z E e E, re)                 -- Terminal case
popRNZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   (r,v) -> (N l e r, v)
popRNZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   (r,v) -> (N l e r, v)
popRNZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   (r,v) -> (N l e r, v)

-- Delete rightmost from (Z l e (Z rl re rr)), height of right sub-tree can't change in this case
-- Don't INLINE this!
popRZZ :: AVL e -> e -> AVL e -> e -> AVL e -> (AVL e,e)
popRZZ l e _  re  E              = (P l e E, re)                 -- Terminal case
popRZZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   (r,v) -> (Z l e r, v)
popRZZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   (r,v) -> (Z l e r, v)
popRZZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   (r,v) -> (Z l e r, v)

-- Delete rightmost from (P l e (Z rl re rr)), height of right sub-tree can't change in this case
popRPZ :: AVL e -> e -> AVL e -> e -> AVL e -> (AVL e,e)
{-# INLINE popRPZ #-}
popRPZ l e _  re  E              = let t = rebalP l e E             -- Terminal case, Needs rebalancing
                                   in  t `seq` (t,re)
popRPZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   (r,v) -> (P l e r, v)
popRPZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   (r,v) -> (P l e r, v)
popRPZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   (r,v) -> (P l e r, v)

-- Delete rightmost from (Z l e (N rl re rr))
-- Don't INLINE this!
popRZN :: AVL e -> e -> AVL e -> e -> AVL e -> (AVL e,e)
popRZN l e rl re rr = case popRN rl re rr of
                      (r,v) -> let t = chkRZ l e r in  t `seq` (t,v)

-- Delete rightmost from (Z l e (P rl re rr))
-- Don't INLINE this!
popRZP :: AVL e -> e -> AVL e -> e -> AVL e -> (AVL e,e)
popRZP l e rl re rr = case popRP rl re rr of
                      (r,v) -> let t = chkRZ l e r in  t `seq` (t,v)
-----------------------------------------------------------------------
-------------------------- popR Ends Here -----------------------------
-----------------------------------------------------------------------

rebalP :: AVL e -> e -> AVL e -> AVL e
rebalP  E                        _ _ = error "rebalP: Bug0"             -- impossible case
rebalP (P ll le lr             ) e r = Z ll le (Z lr e r)               -- P->Z, dH=-1
rebalP (Z ll le lr             ) e r = N ll le (P lr e r)               -- P->N, dH= 0
rebalP (N  _  _  E             ) _ _ = error "rebalP: Bug1"             -- impossible case
rebalP (N ll le (P lrl lre lrr)) e r = Z (Z ll le lrl) lre (N lrr e r)  -- P->Z, dH=-1
rebalP (N ll le (Z lrl lre lrr)) e r = Z (Z ll le lrl) lre (Z lrr e r)  -- P->Z, dH=-1
rebalP (N ll le (N lrl lre lrr)) e r = Z (P ll le lrl) lre (Z lrr e r)  -- P->Z, dH=-1



rebalN :: AVL e -> e -> AVL e -> AVL e
rebalN _ _  E                        = error "rebalN: Bug0"             -- impossible case
rebalN l e (N rl              re rr) = Z (Z l e rl) re rr               -- N->Z, dH=-1
rebalN l e (Z rl              re rr) = P (N l e rl) re rr               -- N->P, dH= 0
rebalN _ _ (P  E               _  _) = error "rebalN: Bug1"             -- impossible case
rebalN l e (P (N rll rle rlr) re rr) = Z (P l e rll) rle (Z rlr re rr)  -- N->Z, dH=-1
rebalN l e (P (Z rll rle rlr) re rr) = Z (Z l e rll) rle (Z rlr re rr)  -- N->Z, dH=-1
rebalN l e (P (P rll rle rlr) re rr) = Z (Z l e rll) rle (N rlr re rr)  -- N->Z, dH=-1


{- --------------- write ------------------ -}

write :: (e -> COrdering e) -> AVL e -> AVL e
write c t = case openPathWith c t of
            FullBP pth e -> writePath pth e t
            _            -> t


-- Complexity: O(log n)
openPathWith :: (e -> COrdering a) -> AVL e -> BinPath a
openPathWith c t = find 1 0 t where
 find  _ i  E        = EmptyBP i
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       Lt   -> let d_ = (d+d) in find d_ (i+d ) l
                       Eq a -> FullBP i a
                       Gt   -> let d_ = (d+d) in find d_ (i+d_) r -- d_ = 2d


writePath :: Int -> e -> AVL e -> AVL e
writePath i0 e' t = wp i0 t where
 wp 0  E        = error "writePath: Bug0" -- Needed to force strictness in path
 wp 0 (N l _ r) = N l e' r
 wp 0 (Z l _ r) = Z l e' r
 wp 0 (P l _ r) = P l e' r
 wp _  E        = error "writePath: Bug1"
 wp i (N l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` N l' e r
                            else let r' = wp (goR i) r in r' `seq` N l  e r'
 wp i (Z l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` Z l' e r
                            else let r' = wp (goR i) r in r' `seq` Z l  e r'
 wp i (P l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` P l' e r
                            else let r' = wp (goR i) r in r' `seq` P l  e r'




-- | Uses the supplied combining comparison to evaluate the union of two sets represented as
-- sorted AVL trees. Whenever the combining comparison is applied, the first comparison argument is
-- an element of the first tree and the second comparison argument is an element of the second tree.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
unionT :: (e -> e -> COrdering e) -> AVL e -> AVL e -> AVL e
unionT c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = t1
 gu t0                 E          = t0
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight 2 l0) t1 (addHeight 2 l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight 2 l0) t1 (addHeight 1 l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight 2 l0) t1 (addHeight 2 r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight 1 l0) t1 (addHeight 2 l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight 1 l0) t1 (addHeight 1 l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight 1 l0) t1 (addHeight 2 r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight 2 r0) t1 (addHeight 2 l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight 2 r0) t1 (addHeight 1 l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight 2 r0) t1 (addHeight 2 r1)
 gu_ t0 h0 t1 h1 = case unionH c t0 h0 t1 h1 of (t,_) -> t






-- | Uses the supplied combining comparison to evaluate the union of two sets represented as
-- sorted AVL trees of known height. Whenever the combining comparison is applied, the first
-- comparison argument is an element of the first tree and the second comparison argument is
-- an element of the second tree.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
-- (Faster than Hedge union from Data.Set at any rate).
unionH :: (e -> e -> COrdering e) -> AVL e -> Int -> AVL e -> Int -> (AVL e,Int)
unionH c = u where
 -- u :: AVL e -> Int -> AVL e -> Int -> (AVL e,Int)
 u  E           _   t1          h1 = (t1,h1)
 u  t0          h0  E           _  = (t0,h0)
 u (N l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 (h0 - 2) e0 r0 (h0 - 1) l1 (h1 - 2) e1 r1 (h1 - 1)
 u (N l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 (h0 - 2) e0 r0 (h0 - 1) l1 (h1 - 1) e1 r1 (h1 - 1)
 u (N l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 (h0 - 2) e0 r0 (h0 - 1) l1 (h1 - 1) e1 r1 (h1 - 2)
 u (Z l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 1) l1 (h1 - 2) e1 r1 (h1 - 1)
 u (Z l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 1) l1 (h1 - 1) e1 r1 (h1 - 1)
 u (Z l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 1) l1 (h1 - 1) e1 r1 (h1 - 2)
 u (P l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 2) l1 (h1 - 2) e1 r1 (h1 - 1)
 u (P l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 2) l1 (h1 - 1) e1 r1 (h1 - 1)
 u (P l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 (h0 - 1) e0 r0 (h0 - 2) l1 (h1 - 1) e1 r1 (h1 - 2)
 u_ l0 hl0 e0 r0 hr0 l1 hl1 e1 r1 hr1 =
  case c e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                                 case forkR r0 hr0 e1 of
          (rl0,hrl0,e1_,rr0,hrr0)  -> case forkL e0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           (ll1,hll1,e0_,lr1,hlr1) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                          case u  l0  hl0 ll1 hll1 of
            (l,hl)                 -> case u rl0 hrl0 lr1 hlr1 of
             (m,hm)                -> case u rr0 hrr0  r1  hr1 of
              (r,hr)               -> case spliceH m hm e1_ r hr of
               (t,ht)              -> spliceH l hl e0_ t ht
  -- e0 = e1
  Eq e ->                case u l0 hl0 l1 hl1 of
          (l,hl)  -> case u r0 hr0 r1 hr1 of
           (r,hr) -> spliceH l hl e r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                                 case forkL e0 r1 hr1 of
          (rl1,hrl1,e0_,rr1,hrr1)  -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           (ll0,hll0,e1_,lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                          case u ll0 hll0  l1  hl1 of
            (l,hl)                 -> case u lr0 hlr0 rl1 hrl1 of
             (m,hm)                -> case u  r0  hr0 rr1 hrr1 of
              (r,hr)               -> case spliceH l hl e1_ m hm of
               (t,ht)              -> spliceH t ht e0_ r hr





 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: e -> AVL e -> Int -> (AVL e,Int,e,AVL e,Int)
 forkL e0 t1 ht1 = forkL_ t1 ht1 where
  forkL_  E        _ = (E, 0, e0, E, 0)
  forkL_ (N l e r) h = forkL__ l (h - 2) e r (h - 1)
  forkL_ (Z l e r) h = forkL__ l (h - 1) e r (h - 1)
  forkL_ (P l e r) h = forkL__ l (h - 1) e r (h - 2)
  forkL__ l hl e r hr = case c e0 e of
                        Lt     ->                            case forkL_ l hl of
                                  (l0,hl0,e0_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                   (l1_,hl1_)         -> (l0,hl0,e0_,l1_,hl1_)
                        Eq e0_ -> (l,hl,e0_,r,hr)
                        Gt     ->                            case forkL_ r hr of
                                  (l0,hl0,e0_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                   (l0_,hl0_)         -> (l0_,hl0_,e0_,l1,hl1)




 --forkR :: AVL e -> Int -> e -> (AVL e,Int,e,AVL e,Int)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        _ = (E, 0, e1, E, 0)
  forkR_ (N l e r) h = forkR__ l (h - 2) e r (h - 1)
  forkR_ (Z l e r) h = forkR__ l (h - 1) e r (h - 1)
  forkR_ (P l e r) h = forkR__ l (h - 1) e r (h - 2)
  forkR__ l hl e r hr = case c e e1 of
                        Lt     ->                            case forkR_ r hr of
                                  (l0,hl0,e1_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                   (l0_,hl0_)         -> (l0_,hl0_,e1_,l1,hl1)
                        Eq e1_ -> (l,hl,e1_,r,hr)
                        Gt     ->                            case forkR_ l hl of
                                  (l0,hl0,e1_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                   (l1_,hl1_)         -> (l0,hl0,e1_,l1_,hl1_)
-----------------------------------------------------------------------
-------------------------- unionH Ends Here ---------------------------
-----------------------------------------------------------------------


-- Splice two AVL trees of known height using the supplied bridging element.
-- That is, the bridging element appears \"in the middle\" of the resulting AVL tree.
-- The elements of the first tree argument are to the left of the bridging element and
-- the elements of the second tree are to the right of the bridging element.
--
-- This function does not require that the AVL heights are absolutely correct, only that
-- the difference in supplied heights is equal to the difference in actual heights. So it's
-- OK if the input heights both have the same unknown constant offset. (The output height
-- will also have the same constant offset in this case.)
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
spliceH :: AVL e -> Int -> e -> AVL e -> Int -> (AVL e,Int)
-- You'd think inlining this function would make a significant difference to many functions
-- (such as set operations), but it doesn't. It makes them marginally slower!!
spliceH l hl b r hr =
 case compare hl hr of
 LT -> spliceHL l hl b r hr
 EQ -> (Z l b r, (hl + 1))
 GT -> spliceHR l hl b r hr

-- Splice two trees of known relative height where hr>hl, using the supplied bridging element,
-- returning another tree of known relative height.
spliceHL :: AVL e -> Int -> e -> AVL e -> Int -> (AVL e,Int)
spliceHL l hl b r hr = let d = (hr - hl)
                       in if d == 1 then (N l b r, (hr + 1))
                                        else spliceHL_ hr d l b r

-- Splice two trees of known relative height where hl>hr, using the supplied bridging element,
-- returning another tree of known relative height.
spliceHR :: AVL e -> Int -> e -> AVL e -> Int -> (AVL e,Int)
spliceHR l hl b r hr = let d = (hl - hr)
                       in if d == 1 then (P l b r, (hl + 1))
                                        else spliceHR_ hl d l b r

-- Splice two trees of known relative height where hr>hl+1, using the supplied bridging element,
-- returning another tree of known relative height. d >= 2
spliceHL_ :: Int -> Int -> AVL e -> e -> AVL e -> (AVL e,Int)
spliceHL_ _  _ _ _  E           = error "spliceHL_: Bug0"          -- impossible if hr>hl
spliceHL_ hr d l b (N rl re rr) = let r_ = spliceLN l b (d - 2) rl re rr
                                  in  r_ `seq` (r_,hr)
spliceHL_ hr d l b (Z rl re rr) = let r_ = spliceLZ l b (d - 1) rl re rr
                                  in case r_ of
                                     E       -> error "spliceHL_: Bug1"
                                     Z _ _ _ -> (r_,        hr )
                                     _       -> (r_,(hr + 1))
spliceHL_ hr d l b (P rl re rr) = let r_ = spliceLP l b (d - 1) rl re rr
                                  in  r_ `seq` (r_,hr)

-- Splice two trees of known relative height where hl>hr+1, using the supplied bridging element,
-- returning another tree of known relative height. d >= 2 !!
spliceHR_ :: Int -> Int -> AVL e -> e -> AVL e -> (AVL e,Int)
spliceHR_ _  _  E           _ _ = error "spliceHR_: Bug0"          -- impossible if hl>hr
spliceHR_ hl d (N ll le lr) b r = let l_ = spliceRN r b (d - 1) ll le lr
                                  in  l_ `seq` (l_,hl)
spliceHR_ hl d (Z ll le lr) b r = let l_ = spliceRZ r b (d - 1) ll le lr
                                  in case l_ of
                                     E       -> error "spliceHR_: Bug1"
                                     Z _ _ _ -> (l_,        hl )
                                     _       -> (l_,(hl + 1))
spliceHR_ hl d (P ll le lr) b r = let l_ = spliceRP r b (d - 2) ll le lr
                                  in  l_ `seq` (l_,hl)
-----------------------------------------------------------------------
-------------------------- spliceH Ends Here --------------------------
-----------------------------------------------------------------------

-- | Uses the supplied combining comparison to evaluate the intersection of two sets represented as
-- sorted AVL trees.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.

intersectionT :: (a -> b -> COrdering c) -> AVL a -> AVL b -> AVL c
intersectionT c t0 t1 = case intersectionH c t0 t1 of (t,_) -> t


intersectionH :: (a -> b -> COrdering c) -> AVL a -> AVL b -> (AVL c,Int)
intersectionH cmp = i where
 -- i :: AVL a -> AVL b -> (AVL c,Int)
 i  E            _           = (E,0)
 i  _            E           = (E,0)
 i (N l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i_ l0 e0 r0 l1 e1 r1 =
  case cmp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
    Lt   ->                     case forkR r0 e1 of
          (rl0,_,mbc1,rr0,_)  -> case forkL e0 l1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           (ll1,_,mbc0,lr1,_) ->                     -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                     case i rr0  r1 of
                    (r,hr)    -> case i rl0 lr1 of
                     (m,hm)   -> case i  l0 ll1 of
                      (l,hl)  -> case (case mbc1 of
                                           Just c1 -> spliceH m hm c1 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       (t,ht) -> case mbc0 of
                                     Just c0 -> spliceH l hl c0 t ht
                                     Nothing -> joinH   l hl    t ht
  -- e0 = e1
    Eq c ->   case i l0 l1 of
               (l,hl)  -> case i r0 r1 of
                           (r,hr) -> spliceH l hl c r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
    Gt   ->                            case forkL e0 r1 of
          (rl1,_,mbc0,rr1,_)  -> case forkR l0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           (ll0,_,mbc1,lr0,_) ->                     -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                                     case i  r0 rr1 of
                    (r,hr)    -> case i lr0 rl1 of
                     (m,hm)   -> case i ll0  l1 of
                      (l,hl)  -> case (case mbc0 of
                                           Just c0 -> spliceH m hm c0 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       (t,ht) -> case mbc1 of
                                     Just c1 -> spliceH l hl c1 t ht
                                     Nothing -> joinH   l hl    t ht

-- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
-- the right order (c e0 e1)
-- forkL :: a -> AVL b -> (AVL b,,Maybe c,AVL b,)
 forkL e0 t1 = forkL_ t1 0 where
  forkL_  E        h = (E,h,Nothing,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l (h-2) e r (h-1)
  forkL_ (Z l e r) h = forkL__ l (h-1) e r (h-1)
  forkL_ (P l e r) h = forkL__ l (h-1) e r (h-2)
  forkL__ l hl e r hr = case cmp e0 e of
                          Lt    ->  case forkL_ l hl of
                                        (l0,hl0,mbc0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                                                         (l1_,hl1_)          -> (l0,hl0,mbc0,l1_,hl1_)
                          Eq c0 -> (l,hl,Just c0,r,hr)
                          Gt    ->   case forkL_ r hr of
                                        (l0,hl0,mbc0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                                                      (l0_,hl0_)          -> (l0_,hl0_,mbc0,l1,hl1)

-- forkR :: AVL a -> b -> (AVL a,,Maybe c,AVL a,)
 forkR t0 e1 = forkR_ t0 0 where
  forkR_  E        h = (E,h,Nothing,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l (h-2) e r (h-1)
  forkR_ (Z l e r) h = forkR__ l (h-1) e r (h-1)
  forkR_ (P l e r) h = forkR__ l (h-1) e r (h-2)
  forkR__ l hl e r hr = case cmp e e1 of
                          Lt    -> case forkR_ r hr of
                                     (l0,hl0,mbc1,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                                                  (l0_,hl0_)          -> (l0_,hl0_,mbc1,l1,hl1)
                          Eq c1 -> (l,hl,Just c1,r,hr)
                          Gt    -> case forkR_ l hl of
                                     (l0,hl0,mbc1,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                                                    (l1_,hl1_)          -> (l0,hl0,mbc1,l1_,hl1_)




-- | Join two AVL trees of known height, returning an AVL tree of known height.
-- It's OK if heights are relative (I.E. if they share same fixed offset).
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
joinH :: AVL e -> Int -> AVL e -> Int -> (AVL e,Int)
joinH l hl r hr =
  case compare hl hr of
       -- hr > hl
      LT -> case l of
              E          -> (r,hr)
              N ll le lr -> case popRN ll le lr of
                              (l_,e) -> case l_ of
                                           Z _ _ _ -> spliceHL l_ (hl-1) e r hr -- dH=-1
                                           _       -> spliceHL l_         hl  e r hr -- dH= 0
              Z ll le lr -> case popRZ ll le lr of
                              (l_,e) -> case l_ of

                                           E       -> pushHL_ l r hr                  -- l had only 1 element
                                           _       -> spliceHL l_         hl  e r hr -- dH=0
              P ll le lr -> case popRP ll le lr of
                                (l_,e) -> case l_ of
                                           Z _ _ _ -> spliceHL l_    (hl-1) e r hr -- dH=-1
                                           _       -> spliceHL l_    hl  e r hr -- dH= 0
              -- hr = hl
      EQ -> case l of
              E          -> (l,hl)              -- r must be empty too, don't use emptyAVL!
              N ll le lr -> case popRN ll le lr of
                                (l_,e) -> case l_ of
                                             Z _ _ _ -> spliceHL l_ (hl-1) e r hr -- dH=-1
                                             _       -> (Z l_ e r, (hr+1))    -- dH= 0
              Z ll le lr -> case popRZ ll le lr of
                              (l_,e) -> case l_ of
                                          E       -> pushHL_ l r hr                 -- l had only 1 element
                                          _       -> (Z l_ e r, (hr+1))    -- dH= 0
              P ll le lr -> case popRP ll le lr of
                              (l_,e) -> case l_ of
                                          Z _ _ _ -> spliceHL l_ (hl-1) e r hr -- dH=-1
                                          _       -> (Z l_ e r, (hr+1))    -- dH= 0

    -- hl > hr
      GT -> case r of
              E          -> (l,hl)
              N rl re rr -> case popLN rl re rr of
                               (e,r_) -> case r_ of
                                           Z _ _ _ -> spliceHR l hl e r_  (hr-1) -- dH=-1
                                           _       -> spliceHR l hl e r_         hr  -- dH= 0
              Z rl re rr -> case popLZ rl re rr of
                                  (e,r_) -> case r_ of
                                              E       -> pushHR_ l hl r                 -- r had only 1 element
                                              _       -> spliceHR l hl e r_ hr          -- dH=0
              P rl re rr -> case popLP rl re rr of
                                (e,r_) -> case r_ of
                                            Z _ _ _ -> spliceHR l hl e r_  (hr-1) -- dH=-1
                                            _       -> spliceHR l hl e r_         hr  -- dH= 0






-- | Push a singleton tree (first arg) in the leftmost position of an AVL tree of known height,
-- returning an AVL tree of known height. It's OK if height is relative, with fixed offset.
-- In this case the height of the result will have the same fixed offset.
--
-- Complexity: O(log n)
--pushHL_ :: AVL e -> AVL e ->  -> (AVL e,)
pushHL_ t0 t h = case t of
               E       -> (t0, (h-1)) -- Relative Heights
               N l e r -> let t_ = putNL l e r in t_ `seq` (t_,h)
               P l e r -> let t_ = putPL l e r in t_ `seq` (t_,h)
               Z l e r -> let t_ = putZL l e r
                          in case t_ of
                             Z _ _ _ -> (t_,         h )
                             P _ _ _ -> (t_, (h-1))
                             _       -> error "pushHL_: Bug0" -- impossible
 where
----------------------------- LEVEL 2 ---------------------------------
--                      putNL, putZL, putPL                          --
-----------------------------------------------------------------------

-- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 putNL  E           e r = Z t0 e r                    -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                         in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                         in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                         in case l' of
                         Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                         P _ _ _ -> Z l' e r         -- L subtree BF:0->+1, H:h->h+1, parent BF:-1-> 0
                         _       -> error "pushHL_: Bug1" -- impossible

-- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 putZL  E           e r = P t0 e r                    -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                         in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                         in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                         in case l' of
                         Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                         N _ _ _ -> error "pushHL_: Bug2" -- impossible
                         _       -> P l' e r         -- L subtree BF: 0->+1, H:h->h+1, parent BF: 0->+1

    -------- This case (PL) may need rebalancing if it goes to LEVEL 3 ---------

-- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 putPL  E           _ _ = error "pushHL_: Bug3"       -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                         in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                        in l' `seq` P l' e r
 putPL (Z ll le lr) e r = putPLL ll le lr e r         -- LL (never returns N)

----------------------------- LEVEL 3 ---------------------------------
--                            putPLL                                 --
-----------------------------------------------------------------------

-- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = Z t0 le (Z lr e r)                  -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putNL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                   in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putPL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                   in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZL lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                   in case ll' of
                                   Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                   N _ _ _ -> error "pushHL_: Bug4" -- impossible
                                   _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+1, H:h->h+1, parent BF:-1->-2, CASE LL !!
-----------------------------------------------------------------------
-------------------------- pushHL_ Ends Here --------------------------
-----------------------------------------------------------------------


-- | Push a singleton tree (third arg) in the rightmost position of an AVL tree of known height,
-- returning an AVL tree of known height. It's OK if height is relative, with fixed offset.
-- In this case the height of the result will have the same fixed offset.
--
-- Complexity: O(log n)
--pushHR_ :: AVL e ->  -> AVL e -> (AVL e,)
pushHR_ t h t0 = case t of
               E         -> (t0, (h-1)) -- Relative Heights
               N l e r -> let t_ = putNR l e r in t_ `seq` (t_,h)
               P l e r -> let t_ = putPR l e r in t_ `seq` (t_,h)
               Z l e r -> let t_ = putZR l e r
                            in case t_ of
                               Z _ _ _ -> (t_,         h )
                               N _ _ _ -> (t_, (h-1))
                               _       -> error "pushHR_: Bug0" -- impossible
 where
----------------------------- LEVEL 2 ---------------------------------
--                      putNR, putZR, putPR                          --
-----------------------------------------------------------------------

-- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 putZR l e E            = N l e t0                    -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                         in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                         in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                         in case r' of
                         Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                         N _ _ _ -> N l e r'         -- R subtree BF: 0->-1, H:h->h+1, parent BF: 0->-1
                         _       -> error "pushHR_: Bug1" -- impossible

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 putPR l e  E           = Z l e t0                    -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                         in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                         in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                         in case r' of
                         Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                         N _ _ _ -> Z l e r'         -- R subtree BF:0->-1, H:h->h+1, parent BF:+1-> 0
                         _       -> error "pushHR_: Bug2" -- impossible

    -------- This case (NR) may need rebalancing if it goes to LEVEL 3 ---------

-- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 putNR _ _ E            = error "pushHR_: Bug3"       -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                         in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                         in r' `seq` N l e r'
 putNR l e (Z rl re rr) = putNRR l e rl re rr         -- RR (never returns P)

----------------------------- LEVEL 3 ---------------------------------
--                            putNRR                                 --
-----------------------------------------------------------------------

-- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re t0                  -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putNR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                   in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putPR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                   in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZR rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                   in case rr' of
                                   Z _ _ _ -> N l e (Z rl re rr')      -- RR subtree BF: 0-> 0, H:h->h, so no change
                                   N _ _ _ -> Z (Z l e rl) re rr'      -- RR subtree BF: 0->-1, H:h->h+1, parent BF:-1->-2, CASE RR !!
                                   _       -> error "pushHR_: Bug4"    -- impossible
-----------------------------------------------------------------------
-------------------------- pushHR_ Ends Here --------------------------
-----------------------------------------------------------------------


-- | Uses the supplied comparison to evaluate the difference between two sets represented as
-- sorted AVL trees. The expression..
--
-- > difference cmp setA setB
--
-- .. is a set containing all those elements of @setA@ which do not appear in @setB@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
differenceT :: (a -> b -> COrdering b) -> AVL a -> AVL b -> AVL a
-- N.B. differenceH works with relative heights on first tree, and needs no height for the second.
differenceT c t0 t1 = case differenceH c t0 0 t1 of (t,_) -> t

differenceH :: (a -> b -> COrdering b) -> AVL a -> Int -> AVL b -> (AVL a,Int)
differenceH comp = d where
 -- d :: AVL a -> Int -> AVL b -> (AVL a,Int)
 d  E           h0  _           = (E ,h0) -- Relative heights!!
 d  t0          h0  E           = (t0,h0)
 d (N l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 (h0-2) e0 r0 (h0-1) l1 e1 r1
 d (N l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 (h0-2) e0 r0 (h0-1) l1 e1 r1
 d (N l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 (h0-2) e0 r0 (h0-1) l1 e1 r1
 d (Z l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-1) l1 e1 r1
 d (Z l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-1) l1 e1 r1
 d (Z l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-1) l1 e1 r1
 d (P l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-2) l1 e1 r1
 d (P l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-2) l1 e1 r1
 d (P l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 (h0-1) e0 r0 (h0-2) l1 e1 r1
 d_ l0 hl0 e0 r0 hr0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt ->                                 case forkR r0 hr0 e1 of
        (rl0,hrl0,    rr0,hrr0)  -> case forkL e0 l1     of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         (ll1,_   ,be0,lr1,_   ) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                           case d rr0 hrr0  r1  of  -- right
          (r,hr)    -> case d rl0 hrl0 lr1  of  -- middle
           (m,hm)   -> case d  l0  hl0 ll1  of  -- left
            (l,hl)  -> case joinH m hm r hr of  -- join middle right
             (y,hy) -> if be0
                           then spliceH l hl e0 y hy
                           else joinH   l hl    y hy
  -- e0 = e1
  Eq _ ->                case d r0 hr0 r1 of -- right
          (r,hr)  -> case d l0 hl0 l1 of -- left
           (l,hl) -> joinH l hl r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt ->                                 case forkL e0 r1     of
        (rl1,_   ,be0,rr1,_   )  -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         (ll0,hll0,    lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                           case d  r0  hr0 rr1  of  -- right
          (r,hr)    -> case d lr0 hlr0 rl1  of  -- middle
           (m,hm)   -> case d ll0 hll0  l1  of  -- left
            (l,hl)  -> case joinH l hl m hm of  -- join left middle
             (x,hx) -> if be0
                           then spliceH x hx e0 r hr
                           else joinH   x hx    r hr
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1), and for other algorithmic reasons in this case.
 -- N.B. forkL returns True if t1 does not contain e0 (I.E. If e0 is an element of the result).
 -- forkL :: a -> AVL b -> (AVL b, Int, Bool, AVL b, Int)
 forkL e0 t1 = forkL_ t1 0 where
  forkL_  E        h = (E,h,True,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l (h-2) e r (h-1)
  forkL_ (Z l e r) h = forkL__ l (h-1) e r (h-1)
  forkL_ (P l e r) h = forkL__ l (h-1) e r (h-2)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt ->                            case forkL_ l hl           of
                              (x0,hx0,be0,x1,hx1) -> case spliceH x1 hx1 e r hr of
                               (x1_,hx1_)         -> (x0,hx0,be0,x1_,hx1_)
                        Eq _  -> (l,hl,False,r,hr)
                        Gt ->                            case forkL_ r hr           of
                               (x0,hx0,be0,x1,hx1) -> case spliceH l hl e x0 hx0 of
                                (x0_,hx0_)         -> (x0_,hx0_,be0,x1,hx1)
 -- N.B. forkR t0, according to e1. Neither of the resulting forks will contain an element
 -- which is "equal" to e1.
 -- forkR :: AVL a -> Int -> b -> (AVL a, Int, AVL a, Int)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        h = (E,h,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l (h-2) e r (h-1)
  forkR_ (Z l e r) h = forkR__ l (h-1) e r (h-1)
  forkR_ (P l e r) h = forkR__ l (h-1) e r (h-2)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt ->                        case forkR_ r hr           of
                              (x0,hx0,x1,hx1) -> case spliceH l hl e x0 hx0 of
                               (x0_,hx0_)     -> (x0_,hx0_,x1,hx1)
                        Eq _  -> (l,hl,r,hr)  -- e1 is dropped.
                        Gt  ->                        case forkR_ l hl           of
                              (x0,hx0,x1,hx1) -> case spliceH x1 hx1 e r hr of
                               (x1_,hx1_)     -> (x0,hx0,x1_,hx1_)
-----------------------------------------------------------------------
----------------------- differenceH Ends Here -------------------------
-----------------------------------------------------------------------
-- | An HAVL represents an AVL tree of known height.
data HAVL e = HAVL (AVL e) Int

-- Local Datatype for results of split operations.
data SplitResult e = All  (HAVL e) (HAVL e)     -- Two tree/height pairs. Non Strict!!
                   | More Int  -- No of tree elements still required (>=0!!)



-- | Split an AVL tree from the Left. The 'Int' argument n (n >= 0) specifies the split point.
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right (l,r)) where l contains
-- the leftmost n elements and r contains the remaining rightmost elements (r will be non-empty).
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
splitAtL :: Int -> AVL e -> Either Int (AVL e, AVL e)
splitAtL n _ | n < 0  = error "splitAtL: Negative argument."
splitAtL 0        E = Left 0       -- Treat this case specially
splitAtL 0        t = Right (E,t)
splitAtL n t = case splitL n t 0 of -- Tree Heights are relative!!
                      More n_                   -> Left (n - n_)
                      All (HAVL l _) (HAVL r _) -> Right (l,r)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where rhavl is empty
splitL :: Int -> AVL e -> Int -> SplitResult e
splitL n  E        _ = More n
splitL n (N l e r) h = splitL_ n l (h-2) e r (h-1)
splitL n (Z l e r) h = splitL_ n l (h-1) e r (h-1)
splitL n (P l e r) h = splitL_ n l (h-1) e r (h-2)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where rhavl is empty
splitL_ :: Int -> AVL e -> Int -> e -> AVL e -> Int -> SplitResult e
splitL_ n l hl e r hr =
 case splitL n l hl of
 More 0         -> let rhavl = pushLHAVL e (HAVL r hr); lhavl = HAVL l hl
                      in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More 1         -> case r of
                      E       -> More 0
                      _       -> let lhavl = pushRHAVL (HAVL l hl) e
                                     rhavl = HAVL r hr
                                 in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More n_           -> let sr = splitL (n_ - 1) r hr
                      in case sr of
                         More _          -> sr
                         All havl0 havl1 -> let havl0' = spliceHAVL (HAVL l hl) e havl0
                                            in  havl0' `seq` All havl0' havl1
 All havl0 havl1   -> let havl1' = spliceHAVL havl1 e (HAVL r hr)
                      in  havl1' `seq` All havl0 havl1'
-----------------------------------------------------------------------
------------------------- splitAtL Ends Here --------------------------
-----------------------------------------------------------------------


pushLHAVL :: e -> HAVL e -> HAVL e
pushLHAVL e (HAVL t ht) = case pushHL e t ht of (t_,ht_) -> HAVL t_ ht_


pushRHAVL :: HAVL e -> e -> HAVL e
pushRHAVL (HAVL t ht) e = case pushHR t ht e of (t_,ht_) -> HAVL t_ ht_


spliceHAVL :: HAVL e -> e -> HAVL e -> HAVL e
spliceHAVL (HAVL l hl) e (HAVL r hr) = case spliceH l hl e r hr of (t,ht) -> HAVL t ht



pushHL :: e -> AVL e -> Int -> (AVL e,Int)
pushHL e t h = pushHL_ (Z E e E) t h

pushHR :: AVL e -> Int -> e -> (AVL e,Int)
pushHR t h e = pushHR_ t h (Z E e E)
