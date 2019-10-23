module OSet (OSet(..),emptySet,setEmpty,inOSet,addOSet,delOSet,inter',union') where
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

inOSet x (OSt (y: [])) = (x==y)
inOSet x (OSt (y:xs))  | (x==y) = true
		       | otherwise = inOSet x xs
			

addOSet x (OSt s) = OSt (add x s) 
   where add x [] = [x]
         add x s@(y:ys) | (x>y)     = y : (add x ys) 
                        | (x<y)     = x : s
                        | otherwise = s
				   
delOSet x (OSt s) = OSt (del x s) 
   where del x [] = []
         del x s@(y:ys) | (x>y) = y : (del x ys)
                        | (x<y) = s
                        | otherwise = ys
			   
--5.4
inter' (OSt []) (OSt []) = (OSt []) 
inter' (OSt []) _  = (OSt []) 
inter' _ (OSt [])  = (OSt []) 
inter' (OSt xt@(x:xs)) (OSt yt@(y:ys)) |x == y    =  addOSet x (inter' (OSt xs) (OSt ys))
                                       |x < y     = inter' (OSt xs) (OSt yt)
								       |otherwise = inter' (OSt xt) (OSt ys) 
							
union'(OSt []) (OSt [])        = (OSt []) 
union' (OSt []) (OSt ys)       = (OSt ys)
union' (OSt xs) (OSt [])       = (OSt xs) 
union' (OSt xs) (OSt (y:ys))   = union' (addOSet y (OSt xs)) (OSt ys)
			   
