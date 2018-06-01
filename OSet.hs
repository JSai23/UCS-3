module OSet (OSet(..),emptySet,setEmpty,inOSet,addOSet,delOSet,inter') where
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
						
--5.4 wrong way
{-
inter' (OSt []) _                                   = [] 
inter' _ (OSt [])                                   = [] 
inter' (OSt (x:[])) (OSt s2)    |ine x s2           = [x]
                                |otherwise          = []
inter' (OSt (x:xs)) (OSt s2)    |ine x s2           = x : inter' (OSt xs) (OSt s2)

ine _ []                     = False
ine a (x:xs)   |a == x       = True
               |a > x        = ine a xs
			   |otherwise    = False 	
-}
			   
--trying the right answer simplify both list at the same time -- output is still wrong is list needs to be ost
inter' (OSt []) (OSt []) = [] 
inter' (OSt []) _  = []
inter' _ (OSt [])  = []
inter' (OSt xt@(x:xs)) (OSt yt@(y:ys)) |x == y    = x : inter' (OSt xs) (OSt ys) --error is because of x: set
                                       |x < y     = inter' (OSt xs) (OSt yt)
								       |otherwise = inter' (OSt xt) (OSt ys) 
			   
