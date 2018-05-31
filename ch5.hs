--5.1
import PQueue
import USet
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
test2 = USt[1,2,3,4,5]
test3 = USt[6,7,6,8,5]


