--ADT for Priority Queue
module PQueue(PQueue(..),pqEmpty,enPQ,enPQ',dePQ,frontPQ) where

emptyPQ :: PQueue a
pqEmpty :: PQueue a -> Bool
enPQ    :: (Ord a) => a -> PQueue a -> PQueue a
enPQ'   :: (Ord a) => a -> PQueue a -> (a -> a -> Bool) -> PQueue a
dePQ    :: (Ord a) => PQueue a -> PQueue a
frontPQ :: (Ord a) => PQueue a -> a

data PQueue a 	= PQ[a]
	deriving Show 

emptyPQ 	  = PQ[]

pqEmpty (PQ [])	  = True 
pqEmpty _ 	  = False 

enPQ x (PQ q) 	  = PQ (insert x q) 
	  where insert x [] 	              = [x]
		insert x r@(e:r') | x <= e    =x:r
						  | otherwise =e:insert x r'
--5.2				  
enPQ' x (PQ q) f = PQ (insert x q f) 
	where 
		insert x [] _	                 =[x]
		insert x r@(e:r') f | f x e          =x:r
							| otherwise      =e:insert x r' f
--end of 5.2 			
			
dePQ (PQ [])	  = error "dePQ: empty priority queue"
dePQ (PQ(x:xs))   = PQ xs

frontPQ (PQ [])	  = error "frontPQ: empty priority queue"
frontPQ (PQ(x:xs))= x
		

