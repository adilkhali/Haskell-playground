module ABR (depth,pre,inf,add,build,build',minTree,delete,splay,acc,access,maxTree,join,somme,rechercher) where

data BT a = N a (BT a) (BT a)
			| E
			deriving (Show,Eq)

depth :: BT a -> Int --profondeur de l'arbre 
depth E = 0
depth (N _ l r) = 1 + max (depth l) (depth r)

pre :: BT a -> [a] --parcours préfixe
pre E = []
pre (N x l r) = [x] ++ pre l ++ pre r

inf :: BT a -> [a] --parcours infixe
inf E = []
inf (N x l r) = inf l ++ [x] ++ inf r

add :: (Ord a) => a -> BT a -> BT a --inserer un élément dans l'arbre
add x E = N x E E 
add x t@(N y l r) | x==y = t
				  | x<y = N y (add x l) r
				  |otherwise = N y l (add x r)

build :: (Ord a) => [a] -> BT a --construire un BT à partir d'une liste
build = foldr add E  -- Arbre dégenérée !!!!!!

build' :: (Ord a) => [a] -> BT a --Si on a une liste triée
build' [] = E
build' xs = N x (build' l) (build' r)
		where
			l = take n xs
			(x:r) = drop n xs
			n = (length xs) `div` 2
				
minTree :: (Ord a) => BT a -> a --le min d'un arbre
minTree (N x E _) = x
minTree (N _ l _) = minTree l

delete :: (Ord a) => a -> BT a -> BT a -- supprimer un élément de l'arbre
delete x (N y E r) | x==y = r
delete x (N y l E) | x==y = l
delete x (N y l r) | x<y = N y (delete x l) r
				   | x>y = N y l (delete x r)
                   | x==y = N k l (delete k r)
						where
							k = minTree r
							
--------- Rotations Zig-Zag : ---------
---------------------------------------
							
splay x (N p t1 (N y t2 t3)) | x==y = N x (N p t1 t2) t3
splay x (N p (N y t1 t2) t3) | x==y = N x t1(N p t2 t3)

splay x (N g (N p (N y t1 t2) t3)t4) | x==y = N x t1 (N g (N p t2 t3) t4)
splay x (N g t1 (N p t2 (N y t3 t4)))| x==y = N x (N g t1(N p t2 t3)) t4
splay x (N g (N p t1 (N y t2 t3))t4) | x==y = N x (N p t1 t2) (N g t3 t4)
splay x (N g t1 (N p (N y t2 t3) t4))| x==y = N x (N g t1 t2) (N p t3 t4) 

splay x t@(N y l r) | x==y = t
splay x E = E
splay x (N y l r) | x<y = splay x (N y (splay x l) r)
				  | x>y = splay x (N y l (splay x r))   
				  
acc x (N y l r) | x==y = x
				| x<y = case l of
						E -> y
						_ -> acc x l
				| x>y = case r of
						E -> y
						_ -> acc x r
						
access x t = splay (acc x t) t

maxTree :: (Ord a) => BT a -> a
maxTree (N x _ E) = x
maxTree (N _ _ r) = maxTree r

join E t = t        --construire un ABR à partir de deux ABR
join t1 t2 = N y t1' t2
	where
		N y t1' E = access (maxTree t1) t1

abr = N 15
		(N 6
			(N 4
				(N 2
					(N 1 E E)
					(N 3 E E)
				)
				(N 5 E E)
			)
			(N 11
				(N 8
					(N 7 E E)
					(N 9 E E)
				)
				(N 13
					(N 12 E E)
					(N 14 E E)
				)
			)
		)
		(N 20
			(N 17 E E)
			(N 22 E E)		
		)
		
somme E = 0
somme (N x l r) = x + somme l +somme r



rechercher _ E = False
rechercher x (N e l r) | x==e = True
					   | x<e = rechercher x l
					   | x>e = rechercher x r
						  						  
tree = N 8
		(N 3
			(N 1 E E)
			(N 6
				(N 4 E E)
				(N 7 E E)
			))
		(N 10
			(E)
			(N 14
				(N 13 E E)
				(E)
				))
				
				