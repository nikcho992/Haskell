delete ::(Eq a) => a -> [a] -> [a]
delete _ [] = []
delete i (x:xs)
  |i == x = xs
  |otherwise = x : delete i xs
  
 
 permutations :: (Eq a)[a] -> [[a]]
 permutations [] = [[]]
 permutations xs = [x:ys | x <- xs, ys <- permutations (delete x xs)]