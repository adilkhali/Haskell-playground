module Graph(Graph,mkGraph,adjacent,sommets,edgesU,edgesD,edgeIn) where
import Data.Array

type Graph n w = Array n [(n,w)]

adjacent :: (Ix n,Num w) => (Graph n w) -> n -> [n]
adjacent g v    = map fst (g!v)

sommets :: (Ix n,Num w) => (Graph n w) -> [n]
sommets g = indices g   -- car le graphe c'est un tableau (indices c'est une fonction de Data.Array qui retourne une liste des indices du tableau)

edgeIn :: (Ix n,Num w) => (Graph n w) -> (n,n) -> Bool
edgeIn g (x,y)  = elem y (adjacent g x) -- vérifier si (x,y) est un arc du graphe

edgesD :: (Ix n,Num w) => (Graph n w) -> [(n,n,w)] -- tous les liens de chaque sommet avec les autres sommets
edgesD g = [(v1,v2,w) | v1<- sommets g , (v2,w) <-g!v1] 

edgesU :: (Ix n,Num w) => (Graph n w) -> [(n,n,w)]
edgesU g = [(v1,v2,w) | v1<- sommets g , (v2,w) <-g!v1 , v1 < v2] 

mkGraph :: (Ix n,Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Graph n w)
mkGraph dir bnds es =
    accumArray (\xs x -> x:xs) [] bnds 
               ([(x1,(x2,w)) | (x1,x2,w) <- es] ++
                if dir then []
                else [(x2,(x1,w))|(x1,x2,w)<-es,x1/=x2])  -- (1,[]) / (1,[(5,78)]) / (1,[(5,78),(3,34)]) / (1,[(5,78),(3,34),(2,12)])
				
graph = mkGraph False (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]