--// =======================================================================//--
--// 						      Adil Khali      					        //--      
--// =======================================================================//--

import qualified Data.Map as M
import Data.List
import Control.Arrow
import Data.Function

data HP a = EH | N a Int (HP a) (HP a)
	deriving (Show)

rang :: HP a -> Int
rang EH = 0
rang (N _ w _ _) = w

size :: HP a -> Int
size EH = 0
size (N x _ l r) = 1 + size l + size r

merge :: (Ord a) => HP a -> HP a -> HP a
merge h EH = h
merge EH h = h
merge (h1@(N x r1 a1 b1)) (h2@(N y r2 a2 b2)) | x<=y = make x a1 (merge b1 h2)
										  |otherwise = make y a2 (merge h1 b2)
										 
										  
make :: (Ord a) =>a -> HP a -> HP a -> HP a
make x a b | rang(a) >= rang(b) = N x (rang(b)+1) a b
		   |otherwise = N x (rang (a)+1) b a
		   
findH :: (Ord a) => HP a -> a
findH EH = error "maxElt of Leaf"
findH (N x _ _ _) = x


insH :: (Ord a) => a -> HP a -> HP a
insH x p = merge (N x 1 EH EH) p

delM ::(Ord a) => HP a -> HP a
delM (N x _ l r) = merge l r

degM :: (Ord a) => (HP a) -> (a,a,HP a)
degM p | (size p) <= 1 = error "aaaaa"
	   |otherwise = (m1,m2,p')
			where
				m1 = findH p
				q = delM p
				m2 = findH q
				p' = delM q


build ::(Ord a) => [a] -> HP a
build = foldr (insH) EH
