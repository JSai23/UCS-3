module USet (USet(..),emptySet,setEmpty,inUSet,addUSet,delUSet,inSet,included,inter,union) where
	--emptySet :: USet a
	--setEmpty :: USet a -> Bool
	--inUSet    :: (Eq a) => a -> USet a -> Bool
	--addUSet   :: (Eq a) => a -> USet a -> USet a
	--delUSet   :: (Eq a) => a -> USet a -> USet a

data USet a = USt[a]
	deriving Show

emptySet = USt []

setEmpty (USt []) = True
setEmpty _ = False

inUSet x (USt xs) = elem x xs

addUSet x s@(USt xs) | inUSet x s = s
				     | otherwise = USt (x:xs)
				   
delUSet x (USt xs) = USt (filter (/= x) xs)

--5.3
inSet (USt xs) x = [] /= [ a |  a <- xs , a == x] 

included (USt s1) (USt s2) = [ a | a <- s1 , b <- s2, a == b] == s1

inter (USt s1) (USt s2) = USt [ a | a <- s1 , b <- s2, a == b] 

union (USt s1) (USt s2) =  [a | a <- s2, not(inSet a (USt s1))] ++ s1

