--CHAPTER 3


--3.1
--A only 
--(f $!a) b
--B only
--((f a) $!b)
--A and B
--((f $!a) $!b)



--3.2
--powers of 2
power x k = if (k ==0)
		then 1
		else if ( k `mod` 2) == 0
			then power (x*x) (k `div` 2)
			else x * (power (x*x) (k `div` 2))
--Number|s(k)
--4     | 13 
--8     | 14
--16    | 18
--32    | 19
--64    |
--128   |
--256   |
--512   |
--1024  |
