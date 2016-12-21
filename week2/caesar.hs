-- Hey yo. Write a function that, given a string and a shift value,
-- returns a new string with all of its characters shifted by the given value.
-- Example:
-- caesarEncode "abc" 2 -> "cde"
-- caesarEncode "blah" 1 -> "cmbi"
-- What are you going to do if the shift value is greater than the alphabet length?
-- For the sake of simplicity, we will work only with lowercase letters from the
-- english alphabet

alphabet :: String
alphabet = ['a'..'z']

nextChar :: Char->Char
nextChar 'z' = 'a'
nextChar c 
  | c `elem` alphabet = succ c
  | otherwise = c

shiftCharBy :: Char->Int->Char
shiftCharBy s 0 = s
shiftCharBy s i = shiftCharBy (nextChar s) (i-1)

caesarEncode :: [Char]->Int->[Char]
caesarEncode [] _ = []
caesarEncode (x:xs) i = (shiftCharBy x i):(caesarEncode xs i ) 

previousChar :: Char->Char
previousChar 'a' = 'z'
previousChar c
  | c `elem` alphabet = pred c
  | otherwise = c

shiftCarBy :: Char->Int->Char
shiftCarBy s 0 = s
shiftCarBy s i
  | i < 0 = shiftCarBy (previousChar s) (i-1)
  | otherwise = shiftCarBy (nextChar s) (i-1) 


  
-- Declare and implement a function, named 'nextChar'
-- Given a character, return the next one.

-- Declare and implement a function, named 'shiftCharBy'
-- Given a character and a shift value(n), get the n-th next character

-- Declare and implement a function, named 'caesarEncode'
-- Given a string and a shift value, get the appropriate caesar encoded string

-- Declare and implement a function, named 'previousChar',
-- which is analogous to 'nextChar'

-- Declare and implement a more general version of 'shiftCarBy'
-- that works with negative shift values too

-- Declare and implement a function, named 'caesarDecode',
-- that given a caesar-encoded string and its shift value, returns the original string

-- Test the following calls to 'caesarDecode'
-- caesarDecode "Mmzzg kpzqabuia ivl pixxg vme gmiz! Mig bpm wlla jm mdmz qv gwcz nidwcz! Mig bpm nwzkm jm eqbp gwc! Ataw, uism aczm gwc omb dmzg dmzg lzcvs!!" 8
-- You know what Santa would cheer if he was black?
-- caesarDecode "pf pf pf!" 9
