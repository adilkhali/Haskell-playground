--// =======================================================================//--
--// 						      Adil Khali      					        //--      
--// =======================================================================//--

import Data.Char
import Data.List
import System.IO

type Mot = String
type Texte = String

main = do
putStrLn "Entrez le nombre des mots que vous voulez Afficher :"
path <- openFile "fichier.txt" ReadMode 
n <- readLn
s <- hGetContents path
showIt (nPremFreq n s) where

-- fonction principale, n premiers mots frequents
nPremFreq :: Int -> Texte -> Mot
nPremFreq n s = concatn (map snd (take n (sortO s)))

-- concatenation
concatn :: [Mot] -> Mot
concatn [] = []
concatn (xs : xss) = xs ++ " " ++ concatn xss

--trier dans un ordre decroissant
sortO :: Texte -> [(Int,String)]
sortO s = reverse (sort (count s))

-- donner les occurences a partir d'un texte 
count :: Texte -> [(Int,Mot)]
count s = occ (sort (words (map toLower s)))

-- associe a chaque mot son occurence
occ :: [Mot] -> [(Int,Mot)]
occ [] = []
occ (x : xs) = [(length (takeWhile (== x) (x : xs)),x)] ++ occ (dropWhile (== x) (x : xs))

--affichage
showIt :: Mot -> IO()
showIt s = do putStr s
