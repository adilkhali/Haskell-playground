--// =======================================================================//--
--// 						      Adil Khali      					        //--      
--// =======================================================================//--

import Data.List
import qualified Data.Map as M 

---------------------------------------------------
--		Codage du Text sous forme binaire		---
---------------------------------------------------

data HF = Leaf Char Int | Node Int HF HF deriving (Show)
type Code = [Char]

instance Eq HF where a == b = fr a == fr b
instance Ord HF where compare a b = compare (fr a) (fr b)

--fonction pour construire un arbre Huffman
huff :: String -> HF
huff s = cons $freqs s

-- retourne la frequence de chaque element
fr :: HF -> Int
fr (Leaf _ x) = x
fr (Node x _ _) = x

-- retourne list de couples : (element,frequence) 
freqs :: String -> [(Char,Int)]
freqs s = M.toList $M.fromListWith (+) $map (flip (,)1) s

-- merge 2 arbres
merge :: HF -> HF -> HF
merge x y = Node (fr x + fr y) x y

-- construire l'arbre a partir de liste triee
cons :: [(Char,Int)] -> HF
cons xs = build $sort $map (trans) xs 

-- build de l'arbre Huffman
build :: [HF] -> HF
build [t] = t
build (x:y:xs) = build $sort ((merge x y) : xs)

-- trnasformer un couple en de (char,Int) a des feuilles
trans :: (Char,Int) -> HF
trans (x,i) = Leaf x i

-- coder un arbre 
coder :: HF -> [(Char,Code)]
coder (Leaf c _) = [(c,"")]
coder (Node _ l r) = map (add "0") (coder l) ++ map (add "1") (coder r) where
																		add c (x,y) = (x,c++y)
-- donner le code d'un texte
encoder :: String -> Code
encoder xs = concat $map (m M.!) xs where m = cMap xs

cMap :: String -> M.Map Char Code
cMap s = M.fromList $coder $huff s

--decoder le text
decoder :: HF -> Code -> String
decoder t xs = dc t xs where
						dc (Leaf c _) [] = [c]
						dc (Leaf c _) bs = c : (dc t bs)
						dc (Node _ l r) (b : bs) = dc (if b == '0' then l else r) bs