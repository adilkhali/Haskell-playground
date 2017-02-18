--// =======================================================================//--
--// 						      Adil Khali      					        //--      
--// =======================================================================//--

module Stack (Stack, empty, isEmpty, top, push, pop) where

newtype Stack a = St [a] 
	deriving (Show)

empty = St []

isEmpty  (St []) = True
isEmpty  _ = False

top (St []) = error "Pile vide !!!"
top (St (x:xs)) = x

push x (St xs) = St (x:xs)

pop (St (x:xs)) = St xs



