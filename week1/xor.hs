import Test.HUnit
-- Implement the exclusive or (xor) function
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- Test cases
tests = test [ "Both arguments hold" ~: "" ~: False ~=? (True `xor` True),
               "Both arguments do not hold" ~: "" ~: False ~=? (False `xor` False),
               "First argument holds" ~: "" ~: True ~=? (True `xor` False),
               "Second argument holds" ~: "" ~: True ~=? (False `xor` True)]
