import Array
module TTable(TTable(..), newTTable, findTTable, updTTable, inTTable) where 
		
data TTable a b = TTbl (Array b a)
	deriving Show

newTTable l        = TTbl (array (lo,hi) l)
	where indices  = map fst l 
          lo       = minimum indices
		  hi       = maximum indices 

findTTable (Tbl a) i = a ! i 

updTable p@(i,x) (Tbl a) = Tbl (a // [p]) 

--not done
inTTable p@(i,x) (Tbl a) |not (i >= minfirst)   = False
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


		  