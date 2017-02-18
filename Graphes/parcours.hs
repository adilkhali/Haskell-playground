import Data.Array
import Data.List
import qualified Data.Map as M

type Graphe = Array Int [Int]

mkGraph :: (Int,Int) -> [(Int,[Int])] -> Graphe
mkGraph bnds es = array bnds es

g = mkGraph (1,4) [(1,[2,3]),(2,[1,3,4]),(3,[1,2]),(4,[2])]

sommets g = indices g 

adjacent g v    = g!v

edgeIn g (x,y)  = elem y (adjacent g x)
								
dfs s g = dfs [s] []
  where
    dfs [] vis    = vis
    dfs (c:cs) vis 
      | elem c vis = dfs cs vis
      | otherwise  = dfs ((adjacent g c)++cs) (vis++[c])
	  
	  
cc g = df(sommets g) [[]]
  where
	df [] cco = cco
	df (c:cs) cco = df (cs\\a) (a:cco)
		where
			a = dfs c g
			
			
fct g = concat(map(\(x,y) -> zip x (repeat y))(zip (cc g) [1..]))
			
ncc g = array (1,n) (fct g)
	where
		n = length(sommets g)
		
		
hasPath :: Int -> Int -> Graphe -> Bool
hasPath x y g = a!x == a!y
	where
		a = ncc g
		

parent g s = f [s] [] ( array(1,length(sommets g))[(i,-1)|i<-sommets g])
			where
				f [] vis t = t
				f(c:cs) vis t | not(elem c vis) = f cs (vis++[c]) (t//[(w,c)|w<-(adjacent g c)\\vis])
							  |elem c vis = f cs vis t
							  
							  
							  
push x xs = (x:xs)
pop (x:xs) = xs
top (x:xs) = x
empty = []
isEmpty xs = length xs == 0 
							
dfs' s g = reverse (dfs (push s empty) [])
 where
   dfs s vis 
    | (isEmpty s)  = vis
    | elem (top s) vis = dfs (pop s) vis
    | otherwise       = let c = top s
                        in dfs (foldr push (pop s) (adjacent g c)) (c:vis) -- foldr f v ==> f = [push (pop s)] <=> ((adjacent g c) ++ cs)
																		   -- puch <=> ++ . et (pop s) <=> cs . et v = (adjacent g c)
																		   -- on fait reverse car dans dfs on a (vis++[c]) et ici on a (c:vis)

emptyQueue = []
queueEmpty xs = length xs == 0 															   
enqueue x xs = xs ++ [x]
dequeue (x:xs) = xs
front (x:xs) = x																												   
																		   
bfs s g = reverse (bfs (enqueue s emptyQueue) [])
 where
  bfs q vis 
   | (queueEmpty q) = vis
   | elem (front q) vis = bfs (dequeue q) vis
   | otherwise  = let c = front q
                  in bfs (foldr enqueue (dequeue q) (adjacent g c)) (c:vis)


							  
						