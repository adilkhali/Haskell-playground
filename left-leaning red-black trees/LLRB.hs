------------------------------------------------------------------
--                 Implementation des LLRB Tree                 --    
--                         Adil Khali                           --
--                      FSR - Master IAO                        --
------------------------------------------------------------------

{----------------------Structure de donnée-------------------------}

data LLRB a = E | N  Color (LLRB a) a (LLRB a)
data Color =  B -- Black / Noir
            | R -- Red / Rouge

{----------------------------Rotation------------------------------}
{-- 
 *
 *       A           B
 *      / \    =>   / \
 *     B               A
 *    / \             / \
--}
rotateL :: LLRB a -> LLRB a
rotateL (N c l x r@(N cr lr xr rr)) = N c (N cr l x lr) xr rr

{-- 
 *
 *       B           A
 *      / \    <=   / \
 *     A               B
 *    / \             / \
--}
rotateR :: LLRB a -> LLRB a
rotateR (N c l@(N cl ll xl lr) x r) = N c ll xl (N cl lr x r)


{------------------------Color Switching---------------------------}

colorSwitch :: LLRB a -> LLRB a
colorSwitch (N c l x r) = switch $ N c (switch l) x (switch r)
colorSwitch _         = error "IMPOSSIBLE  : Empty Node"

{- Changer la couleur en noir -}
toBlack :: LLRB a -> LLRB a
toBlack (N _ l x r) = N B l x r

switch :: LLRB a -> LLRB a
switch (N R l x r)  = N B l x r
switch (N B l x r)  = N R l x r
switch _                = error "IMPOSSIBLE : Empty Leaf"



{-----------------inserer un element dans l'arbre-------------------}


-- rendre la racine Noir car l'insertion se fait avec couleur rouge
insert :: (Ord a) => a -> LLRB a -> LLRB a
insert x t = toBlack (insert' x t)

insert' :: (Ord a) => a -> LLRB a -> LLRB a
insert' x E = N R E x E -- Insertion se fait avec Couleur Rouge
insert' x t@(N c l y r) | x < y     = fix $ N c (insert' x l) y r
                        | x > y     = fix $ N c l y (insert' x r)
                        | otherwise = t -- Lorsque il existe deja 

-- Résoudre tous les problèmes
fix :: LLRB a -> LLRB a
fix t   | bothChildrenRed t = colorSwitch t
        | rightLinkRed t   = rotateL t
        | twoRedLeftInARow t  = colorSwitch $ rotateR t
        | otherwise         = t
{-----------------------------Test--------------------------------}

bothChildrenRed :: LLRB a -> Bool
bothChildrenRed (N _ l@(N R _ _ _) _ r@(N R _ _ _)) = True
bothChildrenRed _                                   = False

rightLinkRed :: LLRB a -> Bool
rightLinkRed (N _ _ _ r@(N R _ _ _)) = True
rightLinkRed _                       = False

twoRedLeftInARow :: LLRB a -> Bool
twoRedLeftInARow (N _ l@(N R ll@(N R _ _ _) _ _) _ _)  = True
twoRedLeftInARow _                                     = False

{------------------------------------------------------------------}

{-- parcours infixe --}
parcours :: LLRB a -> [a]
parcours E            = []
parcours (N _ l x r)  = parcours l ++ [x] ++ parcours r


{-- Construire à partir d'une liste --}
build :: (Ord a) => [a] -> LLRB a
build = foldr insert E

{-- Test si un élément est un membre de l'arbre --}
member :: (Ord a) => a -> LLRB a -> Bool
member _ E = False
member x (N _ l y r) = case compare x y of
                            LT -> member x l
                            GT -> member x r
                            EQ -> True

{-  MINIMUM  -}
minim :: LLRB a -> a 
minim E = error "IMPOSSIBLE DE TROUVER LE MINIMUM DANS UNE ARBRE VIDE"
minim (N _ E x _) = x
minim (N _ l _ _) = minim l

{-  MAXIMUM  -}
maxim :: LLRB a -> a
maxim E = error "IMPOSSIBLE DE TROUVER LE MAXIMUM DANS UNE ARBRE VIDE"
maxim (N _ _ x E) = x
maxim (N _ _ _ r) = maxim r



instance (Show a) => Show (LLRB a) where
    show t =  show $ parcours t

instance Show Color where
    show B = "B"
    show R = "R"