
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)


insert:: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
  where ins x E = T R E x E
        ins x (T c l y r ) | x < y = lbalance c (ins x l) y r
                           | x > y = rbalance c l y (ins x r )
                           | otherwise = T c l y r
        makeBlack E = E
        makeBlack (T c l x r ) = T B l x r



lbalance:: Color -> RBT a -> a -> RBT a -> RBT a 
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B  c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B  c z d)
lbalance c l a r = T c l a r

rbalance:: Color -> RBT a -> a -> RBT a -> RBT a 
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B  c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B  c z d)
rbalance c l a r = T c l a r


type Rank = Int
data Heap a = Empty | N Rank a (Heap a) (Heap a)



merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(N _ x a1 b1 ) h2@(N _ y a2 b2 ) = if x <= y then makeH x a1 (merge b1 h2 )
                                          else makeH y a2 (merge h1 b2 )


rank ::Heap a -> Rank
rank Empty = 0
rank (N r _ _ _) = r


makeH x a b = if rank a > rank b then N (rank b + 1) x a b
              else N (rank a + 1) x b a                            


fromList:: [a] -> Heap a
fromList [] = Empty
fromList [x] = N 0 x Empty Empty
fromList (x:y:xs) = let (x',xs') = (merge (N 0 x Empty Empty) (N 0 y Empty Empty), fromList xs)
                    in merge x' xs'



