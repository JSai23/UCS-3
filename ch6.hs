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
bubblesort [] = []
