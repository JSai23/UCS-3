module OSet (OSet(..),emptySet,setEmpty,inOSet,addOSet,delOSet) where
	--emptySet :: OSet a
	--setEmpty :: OSet a -> Bool
	--inOSet    :: (Eq a) => a -> OSet a -> Bool
	--addOSet   :: (Eq a) => a -> OSet a -> OSet a
	--delOSet   :: (Eq a) => a -> OSet a -> OSet a

data OSet a = OSt[a]
	deriving Show

emptySet = OSt []

setEmpty (OSt []) = True
setEmpty _ = False

inOSet x (OSt s) = elem x (takeWhile (<= x) s) 

addOSet x (OSt s) = OSt (add x s) 
   where add x [] = [x]
         add x s@(y:ys) | (x>y)     = y : (add x ys) 
                        | (x<y)     = s
                        | otherwise = s
				   
delOSet x (OSt s) = OSt (del x s) 
   where del x [] = []
         del x s@(y:ys) | (x>y) = y : (del x ys)
                        | (x<y) = s
                        | otherwise = ys
						
--5.4 
