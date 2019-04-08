import Seq

type Matriz = ((Int,Int),(Int,Int))



fibSeq::Int -> [Int]
fibSeq n = let s = tabulateS f n
               (s',r) = scanS combine neutro s
           in appendS  (drop 1 (map f' s')) (singletonS (f' r))
  where f i = ((1,1),(1,0))
        neutro = ((1,0),(0,1))
        f' ((a,b),(c,d)) = b


combine::Matriz -> Matriz -> Matriz
combine ((a,b),(c,d)) ((x,y),(w,z)) = ((a*x+b*w,a*y+b*z),(c*x+d*w,c*y+d*z))