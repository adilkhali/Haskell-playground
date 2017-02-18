ind xs = (length xs<=1)

divide xs = [take n xs , drop n xs] where n=(length xs) `div` 2

solve = id

combine _ [t1,t2] = merge t1 t2

dpr ind solve divide combine initPb = dc initPb
								where
									dc pb | ind pb = solve pb
										  |otherwise = combine pb (map dc (divide pb))

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x<=y = x:merge xs (y:ys)
					|otherwise = y:merge (x:xs) ys
					
msort xs = dpr ind solve divide combine xs




					

