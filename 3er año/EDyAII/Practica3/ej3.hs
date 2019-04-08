--Ej3:
import Data.List

data CList a = EmptyCL | UnitCL a | Consnoc a (CList a) a deriving Show

cons :: CList a -> a -> CList a
cons EmptyCL x = UnitCL x
cons (UnitCL y) x = Consnoc x EmptyCL y
cons (Consnoc x xs y) x1 = Consnoc x1 (cons xs x ) y

snoc :: CList a -> a -> CList a
snoc EmptyCL x = UnitCL x
snoc (UnitCL y) x = Consnoc y EmptyCL x
snoc (Consnoc x xs y) x1 = Consnoc x (cons xs y) x1



data Shape = Circle Float | Rect Float Float
square::Float -> Shape
square n = Rect n n

area::Shape -> Float
area (Circle r ) = pi * r^2
area (Rect x y ) = pi * y


headCL::CList a -> a
headCL EmptyCL = error "No se puede"
headCL (UnitCL x) = x
headCL (Consnoc x xs y) = x


tailCL::CList a -> CList a
tailCL EmptyCL = error "No se puede"
tailCL (UnitCL x) = EmptyCL
tailCL (Consnoc x xs y) = snoc xs y

isEmpty:: CList a -> Bool
isEmpty EmptyCL = True
isEmpty _ = False

isUnit:: CList a -> Bool
isUnit (UnitCL x) = True
isUnit _ = False

--b)
reverseCL:: CList a ->CList a
reverseCL (Consnoc x xs y) = cons (reverseCL (cons xs x)) y
reverseCL x = x

--c)

initsCL::CList a -> CList (CList a)
initsCL EmptyCL = UnitCL EmptyCL
initsCL (UnitCL x) = Consnoc EmptyCL EmptyCL  (UnitCL x)
initsCL (Consnoc x xs y) = snoc (initsCL(cons xs x)) (Consnoc x xs y)


--d)
lastsCL::CList a -> CList (CList a)
lastsCL EmptyCL = UnitCL EmptyCL
lastsCL (UnitCL x) = Consnoc (UnitCL x) EmptyCL EmptyCL
lastsCL (Consnoc x xs y) = cons (lastsCL(snoc xs y)) (Consnoc x xs y)

--e)
concatCL::CList (CList a) -> CList a
concatCL (UnitCL x) = x
concatCL (Consnoc (UnitCL x) xs y) = cons (concatCL (snoc xs y)) x
concatCL (Consnoc EmptyCL xs y) = concatCL (snoc xs y)
concatCL (Consnoc (Consnoc x xw w) xs y) = cons (concatCL (cons (snoc xs y) (snoc xw w) )) x 