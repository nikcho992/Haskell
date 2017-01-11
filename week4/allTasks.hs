import Data.List

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = length (filter f  xs) > 0

any' f xs = or $ map f xs 

all' f xs = and $ map f xs

split xs num = (take num xs,drop num xs)

--group' :: Eq a => [a] -> [[a]] 

--group' [] = []
--group' [x] = [[x]]
--group' (x:y:xs)
--  | x == y = [x:group' (y:xs)]
--  | otherwise = [x:[]] : group' (y:xs)

 
min' :: (Ord a) => [a] -> a
min' xs = head (sort xs)
-- min' = head . sort
-- min' xs = head $ sort xs 

min2 :: (Ord a) => [a] -> a
min2 [x] = x
min2 (x:y:xs)
  |x < y = min2 (x:xs)
  |otherwise = min2 (y:xs)

min3 :: (Ord a) => [a] -> a
min3 [a] = a
min3 (x:xs) = min x (min3 xs)

min4 :: (Ord a) => [a] -> a
min4 = foldl1 (min)

--TODO
remove _ [] = []
--remove i [i] = []
remove i (x:xs)
  |i == x = xs
  |otherwise = remove i xs