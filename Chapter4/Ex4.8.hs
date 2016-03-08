import Test.QuickCheck
import Test.HUnit

maxThree :: (Integral a) => a -> a -> a -> a
maxThree x y z
    | x >= y && x >= z = x
    | y >= z           = y
    | otherwise        = z

maxThree' :: (Integral a) => a -> a -> a -> a
maxThree' x y z = max (max x y) z

mysteryMax :: (Integral a) => a -> a -> a -> a
mysteryMax x y z
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise      = z

testMaxT1 = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMaxT2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMaxT3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMaxT4 = TestCase (assertEqual "for: maxThree 6 2 6" 6 (maxThree 6 2 6))

testsMaxThree = TestList [testMaxT1, testMaxT2, testMaxT3, testMaxT4]

testMMax1 = TestCase (assertEqual "for: mysteryMax 6 4 1" 6 (mysteryMax 6 4 1))
testMMax2 = TestCase (assertEqual "for: mysteryMax 6 6 6" 6 (mysteryMax 6 6 6))
testMMax3 = TestCase (assertEqual "for: mysteryMax 2 6 6" 6 (mysteryMax 2 6 6))
testMMax4 = TestCase (assertEqual "for: mysteryMax 6 2 6" 6 (mysteryMax 6 2 6))
testMMax5 = TestCase (assertEqual "for: mysteryMax 6 6 2" 6 (mysteryMax 6 6 2))

testsMysteryMax = TestList [testMMax1, testMMax2, testMMax3, testMMax4, testMMax5]

fact :: Int -> Int
fact n
    | n > 1     = n * fact (n-1)
    | otherwise = 1

prop_fact :: Int -> Bool
prop_fact n = fact n > 0

-- 4.33)
allEqual :: (Integral a) => a -> a -> a -> Bool
allEqual x y z
    | x == y && y == z = True
    | otherwise        = False

-- WhiteBox Test Data
testEqual1 = TestCase (assertEqual "for: allEqual 2 2 2" True  (allEqual 2 2 2))
testEqual2 = TestCase (assertEqual "for: allEqual 2 2 3" False (allEqual 2 2 3))
testEqual3 = TestCase (assertEqual "for: allEqual 2 3 3" False (allEqual 2 3 3))
testEqual4 = TestCase (assertEqual "for: allEqual 2 3 2" False (allEqual 2 3 2))
testEqual5 = TestCase (assertEqual "for: allEqual 1 2 3" False (allEqual 1 2 3))
testEqual6 = TestCase (assertEqual "for: allEqual 3 -3 0" False (allEqual 3 (-3) 0))

testsEqual = TestList [testEqual1, testEqual2, testEqual3, testEqual4, testEqual5, testEqual6]

-- 4.34)
solution :: (Integral a) => a -> a -> a -> Bool
solution m n p = ((m+n+p) == 3*p)

-- WhiteBox Test Data
testSolution1 = TestCase (assertEqual "for: solution 2 2 2" True  (solution 2 2 2))
testSolution2 = TestCase (assertEqual "for: solution 2 2 3" False (solution 2 2 3))
testSolution3 = TestCase (assertEqual "for: solution 2 3 3" False (solution 2 3 3))
testSolution4 = TestCase (assertEqual "for: solution 2 3 2" False (solution 2 3 2))
testSolution5 = TestCase (assertEqual "for: solution 1 2 3" False (solution 1 2 3))
testSolution6 = TestCase (assertEqual "for: solution 3 -3 0" False (solution 3 (-3) 0))

testsSolution = TestList [testSolution1, testSolution2, testSolution3, testSolution4, testSolution5, testSolution6]

-- 4.35)
allDifferent :: (Eq a) => a -> a -> a -> Bool
allDifferent x y z
    | x /= y && x /= z && y /= z = True
    | otherwise                  = False

-- BlackBox Test Data
testDifferent1 = TestCase (assertEqual "for: allDifferent 2 2 2" False (allDifferent 2 2 2))
testDifferent2 = TestCase (assertEqual "for: allDifferent 3 2 2" False (allDifferent 3 2 2))
testDifferent3 = TestCase (assertEqual "for: allDifferent 2 3 2" False (allDifferent 2 3 2))
testDifferent4 = TestCase (assertEqual "for: allDifferent 2 2 3" False (allDifferent 2 2 3))
testDifferent5 = TestCase (assertEqual "for: allDifferent 3 2 2" False (allDifferent 3 2 2))
testDifferent6 = TestCase (assertEqual "for: allDifferent 1 2 3" True  (allDifferent 1 2 3))

testsDifferent = TestList [testDifferent1, testDifferent2, testDifferent3, testDifferent4, testDifferent5, testDifferent6]

-- 4.36)
attempt :: (Eq a) => a -> a -> a -> Bool
attempt m n p = (m /= n) && (n /= p)

testAttempt1 = TestCase (assertEqual "for: attempt 2 2 2" False (attempt 2 2 2))
testAttempt2 = TestCase (assertEqual "for: attempt 3 2 2" False (attempt 3 2 2))
testAttempt3 = TestCase (assertEqual "for: attempt 2 3 2" False (attempt 2 3 2))  -- Gotcha!
testAttempt4 = TestCase (assertEqual "for: attempt 2 2 3" False (attempt 2 2 3))
testAttempt5 = TestCase (assertEqual "for: attempt 3 2 2" False (attempt 3 2 2))
testAttempt6 = TestCase (assertEqual "for: attempt 1 2 3" True  (attempt 1 2 3))

testsAttempt = TestList [testAttempt1, testAttempt2, testAttempt3, testAttempt4, testAttempt5, testAttempt6]

-- 4.37)
howManyAboveAverage :: (Fractional a, Ord a) => a -> a -> a -> Integer
howManyAboveAverage x y z
    | x > avg && y > avg && z > avg = 3
    | x > avg && y > avg ||
      x > avg && z > avg ||
      y > avg && z > avg            = 2
    | x > avg || y > avg || z > avg = 1
    | otherwise                     = 0
    where
        avg = (x+y+z)/3

testHMAA1 = TestCase (assertEqual "for: howManyAboveAverage 5 5 5" 0 (howManyAboveAverage 5 5 5))
testHMAA2 = TestCase (assertEqual "for: howManyAboveAverage 1 0 0" 1 (howManyAboveAverage 1 0 0))
testHMAA3 = TestCase (assertEqual "for: howManyAboveAverage 0 1 0" 1 (howManyAboveAverage 0 1 0))
testHMAA4 = TestCase (assertEqual "for: howManyAboveAverage 0 0 1" 1 (howManyAboveAverage 0 0 1))
testHMAA5 = TestCase (assertEqual "for: howManyAboveAverage 1 1 0" 2 (howManyAboveAverage 1 1 0))
testHMAA6 = TestCase (assertEqual "for: howManyAboveAverage 1 0 1" 2 (howManyAboveAverage 1 0 1))
testHMAA7 = TestCase (assertEqual "for: howManyAboveAverage 0 1 1" 2 (howManyAboveAverage 1 1 0))
-- testHMAA8 = TestCase (assertEqual "for howManyAboveAverage " 3 (howManyAboveAverage )) ???

testsHMAA = TestList [testHMAA1, testHMAA2, testHMAA3, testHMAA4, testHMAA5, testHMAA6, testHMAA7]

-- 4.38)
twoExp :: (Integral a, Fractional b) => a -> b
twoExp n
    | n >= 0 = twoExpPositive n
    | n < 0  = 1/twoExpPositive (-n)
    where
        twoExpPositive :: (Integral a, Num b) => a -> b
        twoExpPositive 0 = 1
        twoExpPositive n
            | n `mod` 2 == 0 =   (twoExpPositive (n `div` 2))^2
            | otherwise      = 2*(twoExpPositive ((n-1) `div` 2))^2

testTwoExp1 = TestCase (assertEqual "for: twoExp 0"    1    (twoExp 0))
testTwoExp2 = TestCase (assertEqual "for: twoExp 1"    2    (twoExp 1))
testTwoExp3 = TestCase (assertEqual "for: twoExp 2"    4    (twoExp 2))
testTwoExp4 = TestCase (assertEqual "for: twoExp (-1)" 0.5  (twoExp (-1)))
testTwoExp5 = TestCase (assertEqual "for: twoExp (-2)" 0.25 (twoExp (-2)))

testsTExp = TestList [testTwoExp1, testTwoExp2, testTwoExp3, testTwoExp4, testTwoExp5]

-- 4.39)
-- 4.33')
prop_AE1 :: (Integral a) => a -> a -> a -> Bool
prop_AE1 x y z
    | x == y && y == z           = (allEqual x y z == True)
    | otherwise                  = (allEqual x y z == False)

prop_AE2 :: (Integral a) => a -> a -> a -> Bool
prop_AE2 x y z = allEqual x y z == allEqual z x y &&
                 allEqual z x y == allEqual y z x &&
                 allEqual y z x == allEqual x y z

-- 4.34')
prop_Solution1 :: (Integral a) => a -> a -> a -> Bool
prop_Solution1 x y z
    | x == y && y == z        = (solution x y z == True)
    | solution x y z == True  = (x+y+z) `mod` 3 == 0 && x+y == 2*z
    | solution x y z == False = x+y /= 2*z

prop_Solution2 :: (Integral a) => a -> a -> a -> Bool
prop_Solution2 x y z
    | z == y    = solution x y z == solution z x y
    | x == y    = solution z x y == solution y z x
    | x == z    = solution y z x == solution x y z
    | otherwise = True

-- 4.35')
prop_AD1 :: (Eq a) => a -> a -> a -> Bool
prop_AD1 x y z
    | x /= y && y /= z && x /= z = (allDifferent x y z == True)
    | otherwise                  = (allDifferent x y z == False)

prop_AD2 :: (Eq a) => a -> a -> a -> Bool
prop_AD2 x y z = allDifferent x y z == allDifferent z x y &&
                 allDifferent z x y == allDifferent y z x &&
                 allDifferent y z x == allDifferent x y z

-- 4.37')
prop_HMAA1 :: (Fractional a, Ord a) => a -> a -> a -> Bool
prop_HMAA1 x y z = howManyAboveAverage x y z == howManyAboveAverage z x y &&
                   howManyAboveAverage z x y == howManyAboveAverage y z x &&
                   howManyAboveAverage y z x == howManyAboveAverage x y z

-- 4.38')
-- twoExp() is an odd function on Z \ {0}
prop_TE1 :: (Integral a) => a -> Bool
prop_TE1 n = (twoExp n /= twoExp (-n)) || n == 0

-- The most fundamental property check, because we know that (**) is correct!
prop_TE2 :: Integer -> Bool
prop_TE2 n = twoExp n == 2**(fromInteger n)

-- twoExp() is strictly increasing
prop_TE3 :: (Integral a) => a -> a -> Bool
prop_TE3 n m = (twoExp n < twoExp m) || n >= m

-- Graph Check
prop_TE4 :: Integer -> Bool
prop_TE4 n = sqrt (fromInteger n) < twoExp n || n < 0

--
prop_TE5 :: (Integral a) => a -> a -> Bool
prop_TE5 n m = twoExp (n+m) == twoExp n * twoExp m

prop_TE6 :: (Integral a) => a -> a -> Bool
prop_TE6 n m = twoExp (n-m) == twoExp n / twoExp m
