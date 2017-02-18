ind xs = (length xs<=1)

divide (x:xs) = [filter(<x) xs,filter(>x) xs]

solve = id

combine (x:_) [t1,t2] = t1 ++ [x] ++t2

dpr ind solve divide combine initPb = dc initPb
								where
									dc pb | ind pb = solve pb
										  |otherwise = combine pb (map dc (divide pb))
										  
qsort xs = dpr ind solve divide combine xs




					

