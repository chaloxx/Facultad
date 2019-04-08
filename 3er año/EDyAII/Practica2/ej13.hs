divisors::Int->[Int]
divisors x = if x < 0 then []
			 else [n | n <- [1..x], (mod x) n == 0]


matches::Int->[Int]->[Int]
matches x xs = [n | n <- xs, n == x]


cuadruplas::Int->[(Int,Int,Int,Int)]
cuadruplas x = if x < 0 then []
			   else [(a,b,c,d) | a<-[1..x],b<-[1..x],c<-[1..x],d<-[1..x],a^2+b^2 == c^2+d^2]


unique::[Int]->[Int]
unique xs = [xs !! pos | pos<-[0..(length xs )-1], notElem (xs !! pos) (drop (pos+1) xs)]


--Ejercicio 14

aux::(Int,Int)->Int
aux (x,y) = x*y

scalarproduct::[Int]->[Int]->Int

scalarproduct xs ys = sum (map aux (zip xs ys))