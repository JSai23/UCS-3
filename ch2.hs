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
rep n = [ [x]  | x <- [1..n]] 

