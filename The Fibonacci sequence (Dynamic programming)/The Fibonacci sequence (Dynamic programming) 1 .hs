import Data.Array
fib n = a!n where 
		a = listArray (0,n) [f i | i<-[0..n]] 
		f 0 = 0
		f 1 = 1
		f i = a!(i-1) + a!(i-2)
		
fib' n = l!!n 
	where
		l = 0:1:zipWith (+) l (tail l)
