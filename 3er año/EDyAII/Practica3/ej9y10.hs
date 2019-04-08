{-data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show





insert:: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
             where  ins x E = T R E x E 
                    ins x (T c l y r ) | x < y = lbalance c (ins x l) y r
                                       | x > y = rbalance c l y (ins x r )
                                       |otherwise = T c l y r
                    makeBlack E = E
                    makeBlack (T _ l x r ) = T B l x r



lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r                     = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r                     = T c l a r-}



type Rank = Int 
data Heap a = E | N Rank a (Heap a) (Heap a) deriving Show

rank::Heap a -> Rank
rank E = 0
rank (N r _ _ _ ) = r

makeH::a->Heap a->Heap a ->Heap a
makeH x n m = if rank n > rank m then N (rank m + 1) x n m
              else N (rank n + 1) x m n


merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1 @(N _ x a1 b1 ) h2 @(N _ y a2 b2 ) =  if x <= y then makeH x a1 (merge b1 h2 )
                                               else makeH y a2 (merge h1 b2 )




insert::Ord a =>a->Heap a->Heap a
insert n h = merge (N 1 n E E) h



split::[a]->([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) = let (xs,ys) = split xs
                 in (x:xs,y:ys)

fromList::Ord a =>[a]->Heap a
fromList [] = E
fromList [x] = (N 1 x E E)
fromList (x:y:zs) = let (xs,ys) = split zs
                    in merge (fromList (x:xs)) (fromList (y:ys))

















