-- Chapter 2 Questions
-- Haskell is cruel 
-- #killhaskell
import Array

--2.1 
-- (1+2) -> 3
-- if (2>9) then "Hello" else "Bye" -> "Bye" 
-- let x = (sqrt 16) in (x+1) -> 5


--2.2a
-- The (fact 5) is 120


--2.2b
-- when you evaluate the factorial of 0 you get "Error - C stack overflow" 
fact' 0 = 0
fact' 1 = 1
fact' n = n * fact' (n-1)

--2.2c
fact'' 0 = 1
fact'' 1 = 1
fact'' n = if n < 0 then -1
	   else n * fact'' (n-1)

--2.3 
-- 1:2:3 you cant cons numbers onto numbers
-- actual solution [1, 2, 3]

-- [ [2,3]++[] , [2,3]:[] ] brackets cant enclose a tuple
-- actual solution ([2,3]++[] , [2,3]:[])

-- "hello":"world" can't use cons with strings
-- actual solution "hello" ++ "world" 

--2.4
f l = reverse (f' l [])
	where f' [] r = r
	      f' (x:xs) r = (2*x) : (f' xs r)
-- (f [1,2,3,4]) is [8,6,4,2]


--2.5 
-- [1,2,3] ++ [4] is [1,2,3,4]
-- 1:(2:(3:[4])) is [1,2,3,4]
-- head [1,2,3] is 1
-- tail [1,2,3] is [2,3]
-- drop 4 [1,2,3,4,5] is [5]
-- [1,2,3,4] !! 2 is 3


--2.6a
average [] = 0
average numlist = sum(numlist)/fromIntegral(length(numlist))
		  where
			sum []= 0
			sum (x:xs) = x + sum(xs)
--2.6b
midelem c =  
	(head (drop (div ((length c) - 1)  2)  c)) 



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

