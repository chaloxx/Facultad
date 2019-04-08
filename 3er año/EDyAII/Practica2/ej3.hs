
f::Int->Int
f x = x

ejemplo1a::(Int->Int)-> Int
ejemplo1a f = f 5

ejemplo1b::(Int -> Int)->Int
ejemplo1b f  = f 3


ejemplo2a::Int->(Int->Int)
ejemplo2a x y = x + y

ejemplo2b::Int->Int->Int
ejemplo2b x y = x*y


ejemplo3a::(Int->Int)->(Int->Int)
ejemplo3a f x = f x

ejemplo3b::(Int->Int)->(Int->Int)
ejemplo3b f x = f x + 3


ejemplo4a::Int->Bool
ejemplo4a x = x == 5

ejemplo4b::Int->Bool
ejemplo4b x = x > 6 


ejemplo5a::Bool->(Bool->Bool)
ejemplo5a x y = x == y

ejemplo5b::Bool->(Bool->Bool)
ejemplo5b x y = x > y


ejemplo6a::(Int, Char) -> Bool
ejemplo6a (x,y) = False


ejemplo6b::(Int, Char) -> Bool
ejemplo6b (x,y) = True

ejemplo7a::(Int,Int)->Int
ejemplo7a(x,y) = x + y

ejemplo7b::(Int,Int)->Int
ejemplo7b(x,y) = x*y

ejemplo8a::Int->(Int,Int)
ejemplo8a x = (x,x+1)

ejemplo8b::Int->(Int,Int)
ejemplo8b x = (x,x^3)

ejemplo9a::(Eq a, Num a) => a -> Bool
ejemplo9a x = x == 3


ejemplo9b::(Ord a, Num a) => a -> Bool
ejemplo9b x = x <= 3



ejemplo10a::(Ord a, Num a)  => a->a
ejemplo10a x = x + 1


ejemplo10b::(Ord a, Num a, Show a)  => a->a
ejemplo10b x = x + 1
