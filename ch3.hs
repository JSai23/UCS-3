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
--8     | 18
--16    | 23
--32    | 28
--64    | 33
--128   | 38
--256   | 43
--512   | 48
--1024  | 53
--
--
--K     |s(k)
--8     |23
--9     |24
--10    |24
--11    |25
--12    |24
--13    |25
--14    |25
--15    |26
--
--
--both are O(log n) in big O notation 

--3.3
--im doing this assuming there is storage 
--a. lazy list
--length: n steps O(n)
--sum: mn + n O(mn)
--head: 1 O(1)
--
--b. tails evaluate
--length:m(n-1)+n O(mn)
--sum: mn + n O(mn) 
--head:O(1)

--c. tails and heads evaluated
--length: mn + n O(mn)
--sum: mn+n O(mn)
--head: O(n)

--3.4
--original
prodsumo x = prodo x + sumo x

prodo 0 = 1
prodo n = n * prodo(n-1)

sumo 0 = 0 
sumo n = n + sumo(n-1)

--new definitions of sum and prod that r tail recursive 
prodh 0 = 1
prodh n = prod' n 1

prod' 0 result = result
prod' n result = prod' (n-1) (result *n)

sumh 0 = 1
sumh n = sum' n 0

sum' 0 result = result
sum' n result = sum' (n-1) (result + n)

--new definition of prodsum with tuples (i made it tail recursive)
prodsumtu 0 = 0
prodsumtu x =  z+y where (z,y) = tuplemaker(x)

tuplemaker x = (prodh x, sumh x)
-- new definition of prodsum without tuples tail recursive 
prodsumtr 0 = 0 
prodsumtr x = prodsumtr' x 0 1

prodsumtr' 0 sumacc prodacc = sumacc + prodacc
prodsumtr' n sumacc prodacc = prodsumtr' (n-1) (sumacc + n) (prodacc * n) 



