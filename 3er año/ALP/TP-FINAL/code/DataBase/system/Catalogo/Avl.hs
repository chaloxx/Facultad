{-# LANGUAGE DeriveFoldable , MagicHash #-}


module Avl where
import COrdering
import Data.HashMap.Strict hiding (foldr,map,size,null,join)
import GHC.Exts

(|||) :: a -> b -> (a,b)
a ||| b = (a,b)


data AVL e = E                      -- ^ Empty Tree
           | N (AVL e) e (AVL e)    -- ^ BF=-1 (right height > left height)
           | Z (AVL e) e (AVL e)    -- ^ BF= 0
           | P (AVL e) e (AVL e)    -- ^ BF=+1 (left height > right height)
           deriving(Eq,Ord,Show,Read,Foldable)







empty :: AVL e
empty = E           

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


filterT :: (e -> Bool ) -> AVL e -> AVL e 
filterT f E = E
filterT f t = let (bl,br) = (filterT f $ left t,filterT f $ right t)
                  r    = join bl br
              in case f (value t) of 
                    True -> pushL (value t) r
                    False -> r

merge :: (e -> e -> COrdering e) -> AVL e -> AVL e -> AVL e
merge f t E = t
merge f t1 t2 = let t = merge f t1 (left t2)
                    t' = merge f t (right t2) 
               in push (f (value t2)) (value t2) t'



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
height E = -1
height (N l k r) = let (hl,hr) = (height l) ||| (height r)
                   in 1 + (max hl hr)
height (Z l k r) = let (hl,hr) = (height l) ||| (height r)
                   in 1 + (max hl hr)
height (P l k r) = let (hl,hr) = (height l) ||| (height r)
                   in 1 + (max hl hr)                   


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

  
 
sortedT :: (e -> e -> COrdering e) -> AVL e -> AVL e
sortedT f t = sortedT' f t E
 where sortedT' f E t = t
       sortedT' f t t' = let t1 = sortedT' f (left t) t'
                             t2 = sortedT' f (right t) t1
                         in push (f (value t)) (value t) t2



mapT :: (e -> b) ->  AVL e -> AVL b
mapT _ E = E
mapT f t = let (l',r') = mapT f (left t) ||| mapT f (right t)
           in case t of
                (N _ e _) -> N l' (f e) r' 
                (Z _ e _) -> Z l' (f e) r' 
                (P _ e _) -> P l' (f e) r' 






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
                                    N rll' rle' rlr' -> Z (P l e rll') rle' (Z rlr' re rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
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


join :: AVL e -> AVL e -> AVL e
join t E = t
join E t = t
join t1 t2 = let t = join (left t1) t2
                 t' = join (right t1) t
              in pushL (value t1) t'



particionT :: (a -> Bool) -> AVL a -> (AVL a , AVL a)
particionT p E = (E,E)
particionT p t = let ((l1,l2),(r1,r2)) = particionT p (left t) ||| particionT p (right t)
                     (t1,t2) = join l1 r1 ||| join l2 r2
                 in case p (value t) of
                      True ->  (t1, pushL (value t) t2)
                      False -> (pushL (value t) t1,t2)



-- Compara 2 registros que contengan los atributos k
c :: Ord v => [String] -> HashMap String v -> HashMap String v -> COrdering (HashMap String v)
c k x y = case (x ! (head k)) `compare` (y ! (head k)) of 
           LT -> Lt
           GT -> Gt
           EQ -> case null (tail k) of True -> Eq y
                                       False -> c (tail k) x y


m :: Ord v => [String] -> AVL (HashMap String v) -> AVL (HashMap String v) -> AVL (HashMap String v)
m k = merge (c k)


-- Dado un avl ordenado y uno que tal vez este desordenado, mueve todos los elementos de t2
-- a t1 devolviendo un árbol ordenado
join2 f t E = t
join2 f t1 t2 = let t = join2 f t1 (left t2)
                    t' = join2 f t (right t2)
                    v = value t2
                in push (f v) v t'


eitherMapT :: (e -> Either a b) -> AVL e -> Either a (AVL b)
eitherMapT f E = Right E
eitherMapT f t = do l' <- eitherMapT f (left t)
                    r' <- eitherMapT f (right t)
                    v <- f (value t)
                    case t of 
                       (N l k r) -> return (N l' v r') 
                       (Z l k r) -> return (N l' v r')
                       (P l k r) -> return (N l' v r')


eitherFilterT :: (e -> Either a Bool) -> AVL e -> Either a (AVL e)
eitherFilterT _ E = return E
eitherFilterT f t = do l <- eitherFilterT f (left t)
                       r <- eitherFilterT f (right t)
                       b <- f (value t)
                       let t' = join l r
                       case b of 
                         True -> return $ pushL (value t) t'
                         False -> return $ t'


-- Dados los atributos k y un x definido sobre los atributos k, chequea si x pertenece a t
isMember :: Ord v => [String] -> HashMap String v -> AVL(HashMap String v) -> Bool
isMember k _ E = False
isMember k x t = case c k x  (value t) of 
                     Eq _  -> True 
                     Lt  -> isMember k x (left t)
                     Gt  -> isMember k x (right t) 

 -- Complexity: O(log n)
{-delete :: (e -> Ordering) -> AVL e -> AVL e
delete c t = case findFullPath c t of
              -1# -> t                -- Not found, p<0
              p     -> deletePath p t   -- Found, so delete


addInt x y = (m)+#(n)

-- Complexity: O(log n)
findFullPath :: (e -> Ordering) -> AVL e -> Int#
-- ?? What about strictness if UINT is boxed (i.e. non-ghc)?
findFullPath c t = find 1# 0# t where
 find  _ _  E        = -1#
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       LT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d ) l
                       EQ    -> i
                       GT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d_) r -- d_ = 2d


deletePath :: Int# -> AVL e -> AVL e
deletePath _ E         = error "deletePath: Element not found."
deletePath p (N l e r) = delN p l e r
deletePath p (Z l e r) = delZ p l e r
deletePath p (P l e r) = delP p l e r

----------------------------- LEVEL 1 ---------------------------------
--                       delN, delZ, delP                            --
-----------------------------------------------------------------------

-- Delete from (N l e r)
delN :: Int# -> AVL e -> e -> AVL e -> AVL e
delN p l e r = case sel p of
               LT -> delNL p l e r
               EQ -> subN l r
               GT -> delNR p l e r

-- Delete from (Z l e r)
delZ :: Int# -> AVL e -> e -> AVL e -> AVL e
delZ p l e r = case sel p of
               LT -> delZL p l e r
               EQ -> subZR l r
               GT -> delZR p l e r

-- Delete from (P l e r)
delP :: Int# -> AVL e -> e -> AVL e -> AVL e
delP p l e r = case sel p of
               LT -> delPL p l e r
               EQ -> subP l r
               GT -> delPR p l e r

----------------------------- LEVEL 2 ---------------------------------
--                      delNL, delZL, delPL                          --
--                      delNR, delZR, delPR                          --
-----------------------------------------------------------------------

-- Delete from the left subtree of (N l e r)
delNL :: Int# -> AVL e -> e -> AVL e -> AVL e
delNL p t = dNL (goL p) t
dNL :: Int# -> AVL e -> e -> AVL e -> AVL e
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
delNR :: Int# -> AVL e -> e -> AVL e -> AVL e
delNR p t = dNR (goR p) t
dNR :: Int# -> AVL e -> e -> AVL e -> AVL e
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
delZL :: Int# -> AVL e -> e -> AVL e -> AVL e
delZL p t = dZL (goL p) t
dZL :: Int# -> AVL e -> e -> AVL e -> AVL e
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
delZR :: Int# -> AVL e -> e -> AVL e -> AVL e
delZR p t = dZR (goR p) t
dZR :: Int# -> AVL e -> e -> AVL e -> AVL e
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
delPL :: Int# -> AVL e -> e -> AVL e -> AVL e
delPL p t = dPL (goL p) t
dPL :: Int# -> AVL e -> e -> AVL e -> AVL e
dPL _  E           _ _ = error "delPL: Bug0"             -- Impossible
dPL p (N ll le lr) e r = case sel p of
                         LT -> chkLP  (delNL p ll le lr) e r
                         EQ -> chkLP  (subN    ll    lr) e r
                         GT -> chkLP  (delNR p ll le lr) e r
dPL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` P l' e r  -- height can't change
                         EQ -> chkLP' (subZR ll lr) e r                        -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` P l' e r  -- height can't change
dPL p (P ll le lr) e r = case sel p of
                         LT -> chkLP  (delPL p ll le lr) e r
                         EQ -> chkLP  (subP    ll    lr) e r
                         GT -> chkLP  (delPR p ll le lr) e r

-- Delete from the right subtree of (P l e r)
delPR :: Int# -> AVL e -> e -> AVL e -> AVL e
delPR p t = dPR (goR p) t
dPR :: Int# -> AVL e -> e -> AVL e -> AVL e
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


sel :: Int#-> Ordering
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


goL :: Int# -> Int#
goL p = iShiftRL# p 1#-}