countOccurrences [] = []
countOccurrences (x:xs) = [(x,length (filter(==x) (x:xs)))] ++ countOccurrences(filter (/=x) xs)


countOcc [] x = 0
countOcc (x:xs) y
  |x == y = 1 + countOcc xs y
  |otherwise = countOcc xs y
  
countWithFilter xs y = length (filter (== y) xs)


--[[1,2,3][4,5,6][7,8,9]] 2 -> [3,6,9]

type Matrix a = [[a]]
type Column a = [a]

getNthCol :: Int -> Matrix Int -> Column Int
getNthCol k = map (!!k)