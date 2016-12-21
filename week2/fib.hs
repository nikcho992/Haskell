import Test.HUnit
-- Ahoy! Write a function that returns the n-th fibonacci number
-- By given n. Give it a try using pattern matching.
-- Give it another try using guards.
-- Indexing starts at 0.

--fib :: (Num i,Eq i) => i->Int
--fib 0 = 0
--fib 1 = 1
--fib i = fib(i-1) + fib(i-2)

fib :: (Num i,Eq i) => i->Int
fib i
  | i == 0 = 0
  | i == 1 = 1
  | otherwise = fib(i-1) + fib(i-2)

  
  
  -- Test Cases
-- add more if you wish
tests = test [ "Test for value 2" ~: "" ~: 0 ~=? (fib 0),
               "Test for value 2" ~: "" ~: 1 ~=? (fib 2),
               "Test for value 15" ~: "" ~: 610 ~=? (fib 15)]
