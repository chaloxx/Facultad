data GenTree a = EmptyG | NodeG a [GenTree a] deriving Show
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a) deriving Show


g2bt::GenTree a->BinTree a
g2bt EmptyG = EmptyB
g2bt NodeG r xs = NodeB (g2bt x)  r  EmptyB


g2bt2::[GenTree a]->BinTree a
g2bt2 (NodeG r xs):ys = NodeB (g2bt2 xs) r (g2bt2 ys)
