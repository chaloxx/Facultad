data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp


--a)
eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y) 
eval (Div x y) = div (eval x) (eval y)

--b)
seval:: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = case (seval x, seval y) of 
						(Just x, Just y) -> Just (x*y)
						(_,_) -> Nothing
seval (Div x y) = case (seval x, seval y) of
					   (_, Just 0)-> Nothing
					   (Just x, Just y) -> Just(div x y)
					   (_,_) -> Nothing
