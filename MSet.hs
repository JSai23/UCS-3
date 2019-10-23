module MSet (MSet(..),emptyMSet, msetEmpty, inMSet, addMSet, delMSet, unionMSet, interMSet) where
{--
emptyMSet :: MSet a
msetEmpty :: MSet a -> Bool
inMSet :: Ord a => a -> MSet a -> Bool
addMSet :: Ord a => a -> MSet a -> MSet a
delMSet :: Ord a => a -> MSet a -> MSet a
unionMSet :: Ord a => MSet a -> MSet a -> MSet a
interMSet :: Ord a => MSet a -> MSet a -> MSet a
--}

data MSet a = MSt[(x,y)]
	deriving Show

emptyMSet = MSt []

msetEmpty (MSt []) = True
msetEmpty _        = False

inMSet x (MSt xs) = elem x xs

addMSet x (MSt s) = MSt (add x s) 
   where add x [] = [x]
         add x s@(y:ys) | (x>y)     = y : (add x ys) 
                        | (x<y)     = x : s
                        | otherwise = x : s
				   
delMSet x (MSt xs) = MSt (filter (/= x) xs)

interMSet (MSt []) (MSt []) = (MSt []) 
interMSet (MSt []) _  = (MSt []) 
interMSet _ (MSt [])  = (MSt []) 
interMSet (MSt xt@(x:xs)) (MSt yt@(y:ys)) |x == y    =  addMSet x (interMSet (MSt xs) (MSt ys))
                                       |x < y     = interMSet (MSt xs) (MSt yt)
								       |otherwise = interMSet (MSt xt) (MSt ys) 
							
unionMSet(MSt []) (MSt [])        = (MSt []) 
unionMSet (MSt []) (MSt ys)       = (MSt ys)
unionMSet (MSt xs) (MSt [])       = (MSt xs) 
unionMSet (MSt xs) (MSt (y:ys))   = unionMSet (addMSet y (MSt xs)) (MSt ys)
			   
