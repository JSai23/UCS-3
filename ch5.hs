--5.1
import PQueue
import USet
import OSet
import MSet
import TTable
import Array



data Point a = Pt (a,a) 
	deriving Show
dist (Pt (a,b)) = sqrt((a^2) + (b^2))

instance (Floating a) => Eq (Point a) where
    (==) pa pb = (dist pa) == (dist pb)
instance (Floating a, Ord a) => Ord (Point a) where
    (<=) pa pb	= (dist pa) <= (dist pb)
--5.1 test
a1 = Pt(1.0,1.0)
a2 = Pt(2.0,2.0)
a3 = Pt(3.0,3.0)
test1 = PQ[]

--5.2
--in the PQueue.hs file = enPQ'

--5.3
--all in USet.hs file
test2 = USt[1,2,3,4,5]
test3 = USt[7,6,8,5,4,3,2,1]

--5.4
test4 = OSt[2,3,5,6]
test5 = OSt[2,3,4]

--5.5
test6 = MSt [1,2,3,4,5]

--5.6
x = array (((0),(0)),((1),(1))) [(((0),(0)),(1)),(((0),(1)),(2)), (((1),(0)),(1)), (((1),(1)),(2))]
test7 = TTbl(x)
test8 = array (0,3) [(0, 1),(1,2),(2,3),(3,4)]
test9 = array (0,3) [(0, 1),(1,2),(2,3),(3,4)]
test10 = [1,2,3]
test11 = [1,2,3]

--5.7
data Heap a = EmptyHP | HP a Int (Heap a) (Heap a) deriving Show 
	
emptyHeap = EmptyHP
heapEmpty EmptyHP = True
heapEmpty _  = False

f indHeap EmptyHP  = error "findHeap : empty heap "
f indHeap (HP x _ a b) = x 

rank :: (Ord a) => Heap a -> Int
rank EmptyHP = 0
rank (HP _ r _ _ ) = r

makeHP :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHP x a b 
  | (rank a >= rank b) = HP x (rank b + 1) a b
  | otherwise          = HP x (rank a + 1) b a
			 
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h EmptyHP = h
merge EmptyHP h = h
merge h1@(HP x _ a1 b1) h2@ (HP y _ a2 b2)
		| x <= y = makeHP x a1 (merge b1 h2)
		| otherwise = makeHP y a2 (merge h1 b2)
		
insHeap x h = merge (HP x 1 EmptyHP EmptyHP) h 

delHeap EmptyHP = error " delHeap : empty heap "
delHeap (HP x _ a b) = merge a b 

test10 = [1,2,3]
test11 = [1,2,3]


