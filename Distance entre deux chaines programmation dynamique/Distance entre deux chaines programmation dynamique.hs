import Data.Function
import Data.List
import Data.Array

data Action = Add | Del | Mod | None 
	deriving (Show)

cost :: Action -> Int
cost None = 0
cost _    = 1

dist :: Eq a => [a] -> [a] -> (Int, [Action])
dist a b = (u,w)
   where
		(m,n) = (length a, length b)
		bounds  = ((0,0), (m,n))
		
		ds    = listArray bounds[d i j | (i,j) <- range bounds] 
		 
		f :: Int -> Int -> Action -> (Int, [Action])
		f i j action = (u + cost action, action:v)
			where
				(u,v) = ds !(i,j)
		
		d :: Int -> Int -> (Int, [Action])
		d 0 0 = (0, [])
		d i 0 = f (i-1) 0 Del
		d 0 j = f 0 (j-1) Add
		d i j	| a!!(i-1) == b!!(j-1) = f (i-1) (j-1) None 
				| otherwise    = mini [ f i (j-1) Add,f (i-1) j Del,f (i-1) (j-1) Mod ]
		mini  = minimumBy (compare `on` fst)
		
		(u,v) = d m n
		w = reverse v
