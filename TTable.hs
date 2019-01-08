module TTable(TTable(..), newTTable, findTTable, inTTable, rows, columns, row, col, matMult, arraymult) where 
import Array
		
data TTable a b = TTbl (Array b a)
	deriving Show

newTTable l     = TTbl (array (lo,hi) l)
    where indices      = map fst l 
          lo           = minimum indices
	  hi           = maximum indices 

findTTable (TTbl a) i = a ! i 

updTable p@(i,x) (TTbl a) = TTbl (a // [p]) 


inTTable p@(i,x) (TTbl a) |not (i >= minfirst)   = False
                         |not (i <= maxfirst)   = False
                         |not (x >= minsecond)  = False
			 |not (x <= maxsecond)  = False
			 |otherwise             = True 
   	    where tbounds       = bounds a
		  minbounds     = fst tbounds
		  maxbounds     = snd tbounds
		  minfirst      = fst minbounds
		  minsecond     = snd minbounds
		  maxfirst      = fst maxbounds
		  maxsecond     = snd maxbounds

rows (TTbl a) = (fst (snd (bounds a))) - (fst (fst (bounds a))) 

columns (TTbl a) = (snd (snd (bounds a))) - (snd (fst (bounds a)))

row num (TTbl a) = [a!(num,i) | i <- [0..columns(TTbl a)]]

col num (TTbl a) = [a!(i,num) | i <- [0..rows(TTbl a)]]

matMult (TTbl x) (TTbl y)  = array ((0,0), (rows(TTbl x), columns(TTbl y))) [((i,j), (arraymult (row i (TTbl x)) (col j (TTbl y)))) | i <- [0..(rows (TTbl x))], j <- [0..(columns (TTbl x))]]

arraymult x y = sum([ x!!i * y!!i | i <- [0..((length x) - 1)]])



		  
