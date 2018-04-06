--4.1
concat1 xs = foldr (++) [] xs
concat2 xs = foldl (++) [] xs
-- time efficiency in bigO
-- O(n) 

--4.2
--original 
comp f g l = [f x | x <- (map g l ) , x > 10] 
-- new one pass (check with Dr.Ken) 
comp' f g l = [x | x <- (f (map g l)) , x > 10]

--4.3



