-- Chapter 2 Questions
-- Haskell is cruel 
-- #killhaskell
import Array

--2.2b
fact' 0 = 0
fact' 1 = 1
fact' n = n * fact' (n-1)

--2.2c
fact'' 0 = 0
fact'' 1 = 1
fact'' n = if n < 0 then -1
	   else n * fact'' (n-1)

--2.6a
average [] = 0
average numlist = sum(numlist)/fromIntegral(length(numlist))
		  where
			sum []= 0
			sum (x:xs) = x + sum(xs)

--2.8b
rep 0 = []
rep n = [ x | x <- [1..n], y <- [1..x]] 



--2.9 
charToInt n  
	| (n == '0') = 0
	| (n == '1') = 1
	| (n == '2') = 2
	| (n == '3') = 3
	| (n == '4') = 4
	| (n == '5') = 5
	| (n == '6') = 6
	| (n == '7') = 7
	| (n == '8') = 8
	| (n == '9') = 9
	| otherwise = 0

string2int c  
	| (length(c) > 0) = ((charToInt((head c)) * (10 ^ ((fromIntegral((length c))) - 1)))) + (string2int(tail c))
	| otherwise = 0



--2.11
compose f g x = f (g x) 
-- The type is polymorphic and is (a -> b) -> (c -> a) -> c -> b 





--2.15
-- The types are the following
-- (!) :: Ix a => Array a b -> a -> b
-- bounds :: Ix a => Array a b -> (a,a)
-- indices :: Ix a => Array a b -> [a]
-- elems :: Ix a => Array a b -> [b]

