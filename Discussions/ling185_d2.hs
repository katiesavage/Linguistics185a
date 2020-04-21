module Recursion where
data Numb = E
	  | S Numb
	  deriving (Show, Eq)

isEven :: Numb->Bool
isEven n = case n of
		E -> True
		S n -> not (isEven n)

isOdd :: Numb->Bool
isOdd n = case n of
		E -> False
		S n -> not (isOdd n)

multiply :: [Int] -> Int
multiply n = case n of
		[] -> 1
		n:rest -> n * multiply rest

fib :: Int->Int
fib n = case n of
		0 -> 1
		1 -> 1
		n -> fib(n-1) + fib(n-2)

add :: Numb->Numb->Numb
add a b = case a of
		E -> b
		S a' -> S (add a' b)
				

fibN :: Numb->Numb
fibN n = case n of
		E -> S E
		S n' -> case n' of
			E -> S E
			S'' -> add (fibN n') (fibN n'')

sub :: Numb->Numb->Numb
sub n m = 		
