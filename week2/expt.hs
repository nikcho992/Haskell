import Test.HUnit;
-- Hey yo! Write a function that calculates the x^n by given x and n.
-- Make sure it's fast.
-- x*x*x...*x n times is not fast.

fastExpt :: (Num n) => n -> Int -> n
fastExpt n 0 = 1
fastExpt n k
  | even k = fastExpt (n*n) (k `div` 2)
  | otherwise = n * fastExpt n (k-1) 
  
  -- Test Cases
-- Add more if you wish
tests = test [ "2^5" ~: "" ~: 32 ~=? (fastExpt 2 5),
               "_^0" ~: "" ~: 1 ~=? (fastExpt 21392112 0),
               "4^3" ~: "" ~: 64 ~=? (fastExpt 4 3)]
