import Heap
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
--done
qsort' [] = []
qsort' s =
	(qsort' lower) ++ [(pivotfinder s)] ++ (qsort' upper)
		where 
			  lower = [x | x <- (removeIndex s (div (length s) 2)), x < (pivotfinder s)]
			  upper = [x | x <- (removeIndex s (div (length s) 2)), x  >= (pivotfinder s)]
			  
pivotfinder s = 
	s !! (div (length s) 2)
	
removeIndex xs n = take n xs ++ drop (1 + n) xs

--6.7
--if otherwise counts as a step then this question can be done if not the efficiencey for all lists of the same meaning there is no worst case list
--[8,7,6,5,4,3,2,1]



--6.8 
merge' [] b     = b 
merge'  a []    = a
merge' a@(x:xs) b@(y:ys)
	| (x<=y)     = x : (merge' xs b)
	| otherwise  = y : (merge' a ys) 


msort [] = []
msort [x]  = [x]
msort xs   = merge' (merge' (msort xs1) (msort xs2)) (msort xs3)
	where 
		xs1 = take k xs
		xs2 = drop k (take (2 * k) xs)
		xs3 = drop (2 * k) xs
		k   = ((length xs) `div` 3) +1


test12 = [1,2,3]

--6.9 
--from eimacs converts
listToHeap xs = foldr insHeap EmptyHP xs

test69 = [23,63,21,15,64,96,66,52,20,33,90,19]

{-
 -
HP 15 3 (HP 19 2 (HP 20 2 (HP 33 1 EmptyHP EmptyHP) (HP 52 1 EmptyHP EmptyHP)) (HP 64 1 (HP 66 2 (HP 90 1 EmptyHP EmptyHP) (HP 96 1 EmptyHP EmptyHP)) EmptyHP)) (HP 21 2 (HP 63 1 EmptyHP EmptyHP) (HP 23 1 EmptyHP EmptyHP)) 

-}   
		

