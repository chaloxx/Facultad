module ArrSeq  where


import Seq
import qualified Arr as A

instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = fromListS [x]

    lengthS s = A.length s

    nthS s n = s A.! n

    tabulateS f n = A.tabulate f n

    mapS f s = let f'= \i -> f (nthS s i)
                   n = lengthS s
               in tabulateS f' n

    filterS f s = let f' x = if (f x) then (singletonS x) else emptyS
                      s' = mapS f' s
                  in joinS s'

    fromListS xs = A.fromList xs

    appendS s1 s2 = let l1 = lengthS s1 
                        l2 = lengthS s2
                        f x = if x < l1 then (nthS s1 x) else (nthS s2 (x-l1))
                    in tabulateS f (l1+l2)


    takeS s n = A.subArray 0 n s

    dropS s n = A.subArray n ((lengthS s)-n) s

    showtS s = case (lengthS s) of
               0 -> EMPTY
               1 -> ELT (nthS s 0)
               l -> let m = div l 2
                    in NODE (takeS s m) (dropS s m)

    showlS s = case (lengthS s) of
               0 -> NIL
               l -> CONS (nthS s 0) (dropS s 1)

    joinS s = A.flatten s


    reduceS f b s = case (lengthS s) of
                    0 -> b
                    1 -> f b (s A.! 0)
                    l -> reduceS f b (contractS f s)


    scanS f b s = case (lengthS s) of
                    0 -> (emptyS,b)
                    1 -> (singletonS b,f b (s A.! 0))
                    l -> let s' = contractS f s
                             (s'',r) = scanS f b s'
                         in (expandS f s s'',r)

    contractS f s = case (A.length s) of
               1 -> s
               l -> let  m = ceiling ((fromIntegral l)/2)
                         f' x | x /= (m-1) = f (s A.! (2*x)) (s A.! (2*x+1))
                              | otherwise = if even l then f (s A.! (l-2)) (s A.! (l-1))
                                            else (s A.! (l-1))
                    in A.tabulate f' m
    
    expandS f s s' = let f' x = let m = div x 2 in
                                if even x then s' A.! m
                                else f (s' A.! m) (s A.! (x-1))
                     in A.tabulate f' (A.length s)  
                   
















