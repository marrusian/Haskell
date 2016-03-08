-- 3.9)
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (n == p)

-- 3.10)
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q = (m == n) && (n == p) && (p == q)

fourEqual2 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual2 m n p q = threeEqual m n p && (p == q)

-- 3.11)
{-
threeEqual (2+3) 5 (11 `div` 2)
((2+3) == 5) && (5 == (11 `div` 2))    (definition of threeEqual)
((2+3) == 5) && (5 == 5)               (arithmetic)
((2+3) == 5) && True                   (boolean)
(5 == 5) && True                       (arithmetic)
True && True                           (boolean)
True                                   (boolean)

threeDifferent (2+4) 5 (11 `div` 2)
((2+4) /= 5) && (5 /= (11 `div` 2)) && ((2+4) /= (11 `div` 2))   (definition of threeDifferent)
((2+4) /= 5) && (5 /= (11 `div` 2)) && ((2+4) /= 5)
((2+4) /= 5) && (5 /= (11 `div` 2)) && (6 /= 5)
((2+4) /= 5) && (5 /= (11 `div` 2)) && True
((2+4) /= 5) && (5 /= 5) && True
((2+4) /= 5) && False && True
(6 /= 5) && False && True
True && False && True
True && False
False

fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11)
((2+3) == 5) && (5 == (11 `div` 2)) && ((11 `div` 2) == (21 `mod` 11))
((2+3) == 5) && (5 == (11 `div` 2)) && ((11 `div` 2) == 10)
((2+3) == 5) && (5 == (11 `div` 2)) && (5 == 10)
((2+3) == 5) && (5 == (11 `div` 2)) && False
((2+3) == 5) && (5 == 5) && False
((2+3) == 5) && True && False
(5 == 5) && True && False
True && True && False
True && False
False
-}

-- 3.12)
prop_fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
prop_fourEqual m n p q = fourEqual m n p q == fourEqual2 m n p q
