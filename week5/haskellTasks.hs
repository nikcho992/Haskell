makePairs :: [a] -> [[a]]
makePairs [] = []
makePairs [a] = [[a]]
makePairs (x:y:xs) = [x,y]:makePairs xs

--makePairs2 :: [a] -> [[a]]
makePairs2 [] = []
makePairs2 [a] = [[a]]
makePairs2 xs = [take 2 xs] ++ makePairs2 (drop 2 xs)

interval :: [(Int,Int)] -> (Int,Int)
interval xs = (minimum (map fst xs),maximum (map snd xs))

interval2 xs = (foldl (\ acc pair -> if acc > (fst pair) then fst pair else acc) (fst (head xs)) xs,
				foldl (\ acc pair -> if acc < (snd pair) then snd pair else acc) (snd (head xs)) xs )
				
pref :: [a] -> [[a]]
pref xs = [take x xs | x <- [1 .. length xs]]
				
suff :: [a] -> [[a]]
suff xs = [drop x xs | x <- [0 .. length xs - 1]]

sublist :: (Eq a) => [a] -> [a] -> Bool
sublist _ [] = False
sublist xs ys = elem xs (pref ys) || sublist xs (tail ys)
				
