--6.1 
isort [] =[]
isort (x:xs) = insert x (isort xs)
insert x xs = takeWhile ((<=)x) xs ++ [x] ++ dropWhile ((<=)x) xs
list1 = [4,2,5,6,10,3,7]

--6.2
list2 = [("Aam",3),("Bob",2),("Sam",4)]
--no because isort sorts it based on the first value of the because that is how 
--tuple comparison works

--6.3
bubblesort n [] = []
bubblesort n xs 
	| n == 0    = xs
	| otherwise = bubblesort (n - 1) (bubblesort' xs)

bubblesort' (x:y:xs) 
	| x > y  = y : bubblesort'(x:xs)
	| x <= y = x : bubblesort'(y:xs)
bubblesort' (x) = (x)	

--6.3b
test2 = [1,5,2,3,4]

bubblesort'' n [] = []
bubblesort'' n xs 
	| n == 0                    = xs
	| (bubblesort''' xs) == xs  = xs
	| otherwise                 = bubblesort'' (n - 1) (bubblesort''' xs)

bubblesort''' (x:y:xs) 
	| x > y  = y : bubblesort'''(x:xs)
	| x <= y = x : bubblesort'''(y:xs)
bubblesort''' (x) = (x)	

--6.4

--6.5
qsort n [] = []
qsort n (pivot:rest) 
	|(length (pivot:rest)) <=  n = reverse(isort (pivot:rest))
	|otherwise      =  (qsort  n lower) ++ [pivot] ++ (qsort n upper)
		where 
			  lower = [x | x <- rest, x < pivot]
			  upper = [x | x <- rest, x  >= pivot]
			  
--6.6
--drop the object of pivot from list write function to do that 
qsort' s =
	(qsort' lower) ++ [(pivotfinder s)] ++ (qsort' upper)
		where 
			  lower = [x | x <- (removeIndex s (div (length s) 2)), x < (pivotfinder s)]
			  upper = [x | x <- (removeIndex s (div (length s) 2)), x  >= (pivotfinder s)]
			  
pivotfinder s = 
	s !! (div (length s) 2)
	
removeIndex xs n = fst x ++ snd x
    where x = (take (n-1) xs, drop n xs)
    
		

