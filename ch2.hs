-- Chapter 2 Questions
-- Haskell is cruel 
-- #killhaskell
import Array

--2.1 
-- (1+2) -> 3
-- if (2>9) then "Hello" else "Bye" -> "Bye" 
-- let x = (sqrt 16) in (x+1) -> 5.0


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

-- [ [2,3]++[] , [2,3]:[] ] cant have a list with one list and one list of lists
-- actual solution [[2,3]++[] , [2,3]++[]]
-- "hello":"world" can't cons different datatype a string is a list of characters so u cant cons a string into a string u can only cons characters into a string but u can append a string into a string because ur consing a list of characters into a list of characters 
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
average numlist = sum(numlist)/fromIntegral(length(numlist)) --don't need sum, sum is predefined 
		  where
			sum []= 0
			sum (x:xs) = x + sum(xs)
--2.6b
midelem c =  
	(head (drop (div ((length c) - 1)  2)  c))

--2.7a
--[ (x,y) | x <- [1..2], y <- [2..5], (x+y) /= 4] 
--Above result = [(1,2),(1,4),(1,5),(2,3),(2,4),(2,5)]
--[x | x <- [1..10] , (x `mod` 2 ) == 0]
--Above Result = [2,4,6,8,10]


--2.7b
--[x | x <- [1..15], x /= 9]
--[if (x `mod` 2) == 0 then x else (x*(-1)) | x <- [2..11] ] - first way to solve second one
--[x * (-1)^x | x <- [2..11] ] - more efficient mathematical way to solve second one 

--2.8a
neg c =
	length([ x | x <- c , x < 0])


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

--2.10
--map fst [(1,2),(3,8),(0,6),(3,1)]
--result: [1,3,0,3]
--z x y = (x+y) `div` 2 (foldr z 0 l, foldl z 0 l) where l = [6,9,8,3,10] 
--result: (6, 7)
--foldr (++) [] [ [1,2,3],[4,5,6],[], [7]]
--result: [1,2,3,4,5,6,7]



--2.11
compose f g x = f (g x) 
-- The type is polymorphic and is (a -> b) -> (c -> a) -> c -> b 

--2.12a
--array (1,4) [(1,11),(2,20),(3,36),(4,47)]
--2.12b
--array (1,14) [(x, (if (x>=9) then x+1 else x)) | x <- [1..14] ] 
--2.12c
--array (1,10) [(x-1, (if (x `mod` 2) == 0 then x else (x*(-1)))) | x <- [2..11] ]
--array (1,10) [(x-1, (x * (-1)^x) | x <- [2..11] ]
--the number is equal to n n * (-1)^nth power would get negative for odd and positive for even 

--2.13a
myarray1 = array ((1,1), (3,3)) [((1,1),2), ((1,2),3), ((1,3),4), ((2,1),5), ((2,2),6), ((2,3),7), ((3,1),8), ((3,2),9), ((3,3),10)]
--2.13b
myarray2 = array ((1,1), (3,3)) [((x,y), ((((x-2)*3)+4)+y)) | x <-[1..3], y <-[1..3]]
--2.13c
transpose3 x = 
	x//[((1,2),num2), ((2,1),num1), ((1,3),num4), ((3,1), num3), ((2,3), num6), ((3,2),num5)] where
	num1 = x!(1,2)
	num2 = x!(2,1)
	num3 = x!(1,3)
	num4 = x!(3,1)
	num5 = x!(2,3)
	num6 = x!(3,2)
--2.13d
transposeall a =array (bounds a) [((y,x), a!(x,y))| x<-[1..num1], y<-[1..num2]] where
			num1= fst(snd(bounds a))
			num2= snd(snd(bounds a))

--2.14
cube x = x * x * x
--Num a => a -> a
maxi x y | x >=y = x
	 | otherwise = y 
--Ord a => a -> a -> a
sumAtoB a b = sum [a..b]
--(Num a, Enum a) => a -> a -> a

	

	

--2.15
-- The types are the following
-- (!) :: Ix a => Array a b -> a -> b
-- bounds :: Ix a => Array a b -> (a,a)
-- indices :: Ix a => Array a b -> [a]
-- elems :: Ix a => Array a b -> [b]

