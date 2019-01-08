module Heap(Heap , emptyHeap , heapEmpty , findHeap , insHeap , delHeap)

data (Ord a) => Heap a = EmptyHP | HP a Int (Heap a) (Heap a) deriving Show 
	
empty Heap = EmptyHP
heapEmpty EmptyHP = True
heapEmpty _  = False

f indHeap EmptyHP  = error "findHeap : empty heap "
f indHeap (HP x _ a b) = x 

rank :: (Ord a) => Heap a -> Int
rank EmptyHP = 0
rank (HP _ r _ _ ) = r

makeHP :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHP x a b | rank a >= rank b HP x (rank b + 1) a b
			 | otherwise = HP x (rank a + 1) b a
			 
merge : : (Ord a) => Heap a -> Heap a -> Heap a
merge h EmptyHP = h
merge EmptyHP h = h
merge h1@(HP x _ a1 b1) h2@ (HP y _ a2 b2)
		| x <= y = makeHP x a1 (merge b1 h2)
		| otherwise = makeHP y a2 (merge h1 b2)
		
insHeap x h = merge (HP x 1 EmptyHP EmptyHP) h 

delHeap EmptyHP = error " delHeap : empty heap "
delHeap (HP x _ a b) = merge a b 