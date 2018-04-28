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
comp' f g [] = []
comp' f g l = comphelp f g l [] 

comphelp f g [] acc = reverse acc 
comphelp f g l acc  
  | (g (head l)) > 10 = comphelp f g (tail l) ((f (g (head l))) : acc)
  | otherwise = comphelp f g (tail l) acc
--4.3
--
--original 
split x l = ([y | y <- l, y <=x], [y| y <- l, y > x])
--one pass (i think it is tail recursive)
split' x [] = ([],[])
split' x l = splithelp x l [] [] 

splithelp x [] lessacc moreacc = (reverse(lessacc), reverse(moreacc)) 
splithelp x l lessacc moreacc = if head(l) <= x 
		then splithelp x (tail(l)) ((head(l)) : lessacc) moreacc 
		else splithelp x (tail(l)) lessacc ((head(l)) : moreacc)
--tail recursive 
-- Dr.ken said its not really recursive at all but it counts 
split'' x l = ([y | y <- l, y <=x], [y| y <- l, y > x])


--4.5 
data BinTree a = Empty | NodeBT a (BinTree a) (BinTree a)
		  deriving Show 

treecompare Empty Empty = True 
treecompare Empty (NodeBT v lf rt) = False
treecompare (NodeBT v lf rt) Empty = False 
treecompare (NodeBT v1 lf1 rt1) (NodeBT v2 lf2 rt2) = (((v1==v2) && (treecompare lf1 lf2)) && (treecompare rt1 rt2))

a1 = (NodeBT 3 (NodeBT 2 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 2 (NodeBT 1 Empty Empty) (NodeBT 1 Empty Empty)))
a2 = (NodeBT 3 (NodeBT 2 (NodeBT 1 Empty Empty) (NodeBT 1 Empty Empty)) (NodeBT 2 (NodeBT 1 Empty Empty) (NodeBT 1 Empty Empty)))

--4.7 
data BinTree'' a = Leaf'' a 
		|NodeBT'' (BinTree'' a) (BinTree'' a)
		deriving Show

ff t = (flipT . flipT) t
flipT (NodeBT'' a b) = NodeBT'' (flipT b) (flipT a)
flipT (Leaf'' a) = Leaf'' a

a3 = Leaf'' 3
a4 = NodeBT'' (Leaf'' 1) (Leaf'' 2)
a5 = NodeBT'' (NodeBT'' (Leaf'' 1) (Leaf'' 2))  (NodeBT'' (Leaf'' 3) (Leaf'' 4))

--ff reverses the whole tree
ff' (NodeBT'' a b) = (NodeBT'' a b)
