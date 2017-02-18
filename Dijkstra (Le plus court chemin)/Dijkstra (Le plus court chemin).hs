-- Ici on utilise par exemple Data.List.Key , il faut d'abord télécharger le package de ce lien: http://hackage.haskell.org/package/utility-ht-0.0.11/utility-ht-0.0.11.tar.gz
-- Installation du package à partir du cmd (Windows): 
-- 1- ouvrir le dossier téléchargé contenant la fichier Setup.lhs
-- 2- taper la commande : runhaskell Setup.hs configure 
-- 3- taper la commande : runhaskell Setup.hs build
-- 4- taper la commande : runhaskell Setup.hs install

import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)


--Pour simplifier le reste de l'algorithme, on convertit la liste des arcs 
--en une map qui contient tous les voisins de chaque sommet

buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])] -- ici on travaille avec des graphes non orientés



dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) 
                | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e
			  
--Trouver le chemin le plus court est maintenant facile, en traçant le chemin à partir du du sommet de déstination jusqu'au sommet de départ
			  
shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
	
main :: IO ()
main = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3)
                             ,('b','d',8), ('c','d',7), ('c','e',5)
                             ,('d','e',10)]
          print $ shortestPath 'a' 'e' g 