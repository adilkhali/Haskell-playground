module Graph(Graph,mkGraph,adjacent,sommets,edgesU,edgesD,edgeIn) where
import Data.Array

type Graph n w = Array (n,n) (Maybe w)

mkGraph dir bnds@(l,u) es 
    = emptyArray // ([((x1,x2),Just w) |(x1,x2,w)<-es] ++
                     if dir then []
                     else [((x2,x1),Just w) |(x1,x2,w)<-es,x1/=x2])
      where
      emptyArray
          = array ((l,l),(u,u)) [((x1,x2),Nothing) | x1 <- range bnds, 
                                                     x2 <- range bnds]

sommets g = range (l,u) where ((l,_),(u,_)) = bounds g

edgeIn g (x,y)= (g!(x,y)) /= Nothing  -- vérifier si (x,y) est un arc du graphe
													 
adjacent g v1 = [ v2 | v2 <-sommets g,(g!(v1,v2))/= Nothing]


edgesD g= [(v1,v2,unwrap(g!(v1,v2))) 
                      | v1 <-sommets g, v2 <- sommets g,
                        edgeIn g (v1,v2)]
    where unwrap (Just w) = w
	
edgesU g= [(v1,v2,unwrap(g!(v1,v2)))
                   | v1 <-sommets g, v2 <- range (v1,u),
                     edgeIn g (v1,v2)]
    where (_,(u,_)) = bounds g          
          unwrap (Just w) = w
		  
graph = mkGraph True (1,5) [(1,2,10),(1,3,20),(2,4,30),(3,4,40),(4,5,50)]
