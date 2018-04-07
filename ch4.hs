--4.1
concat1 xs = foldr (++) [] xs
concat2 xs = foldl (++) [] xs
-- time efficiency in bigO is wrong 
-- O(n) 
-- n = length xs 

--4.2
--original 
comp f g l = [f x | x <- (map g l ), x > 10] 
-- new one pass (check with Dr.Ken)  
comp f g [] = []
--comp f g l = 
--4.3
--
--original 
split x l = ([y | y <- l, y <=x], [y| y <- l, y > x])
--one pass (i think it is tail recursive)
split' x [] = ([],[])
split' x l = splithelp x l [] [] 

splithelp x [] lessacc moreacc = (lessacc, moreacc) 
splithelp x l lessacc moreacc = if head(l) <= x 
		then splithelp x (tail(l)) ((head(l)) : lessacc) moreacc 
		else splithelp x (tail(l)) lessacc ((head(l)) : moreacc)
--tail recursive 


