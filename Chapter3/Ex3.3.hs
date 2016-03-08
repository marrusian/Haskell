import Data.List -- maximum(), maximum()

max2 :: (Ord a) => a -> a -> a
max2 x y = if x>y then x else y

maxThree :: (Ord a) => a -> a -> a -> a
maxThree x y z
    | x >= y && x >= z = x
    | y >= z           = y
    | otherwise        = z

prop_compareMax :: (Ord a) => a -> a -> Bool
prop_compareMax x y = max x y == max2 x y

prop_max1, prop_max2 :: (Ord a) => a -> a -> Bool
prop_max1 x y = (x <= max x y) && (y <= max x y)
prop_max2 x y = (x == max x y) || (y == max x y)

-- 3.13)
{-
max (3-2) (3*8)
    ?? (3-2) >= (3*8)
    ?? 1 >= 24
    ?? False
24

maxThree (4+5) (2*6) (100 `div` 7)
    ?? (4+5) >= (2*6) && (4+5) >= (100 `div` 7)
    ?? 9 >= 12 && 9 >= 14
    ?? False && False
    ?? False
    ?? 12 >= 14
    ?? False
14
-}

-- 3.14)
min2 :: (Ord a) => a -> a -> a
min2 x y = if x <= y then x else y

minThree :: (Ord a) => a -> a -> a -> a
minThree x y z
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

-- 3.15)
prop_compareMin :: (Ord a) => a -> a -> Bool
prop_compareMin x y = min x y == min2 x y

prop_min1, prop_min2 :: (Ord a) => a -> a -> Bool
prop_min1 x y = (x >= min2 x y) && (y >= min2 x y)
prop_min2 x y = (x == min2 x y) || (y == min2 x y)

prop_compareMaxThree :: (Ord a) => a -> a -> a -> Bool
prop_compareMaxThree x y z = maxThree x y z == maximum [x,y,z]

prop_compareMinThree :: (Ord a) => a -> a -> a -> Bool
prop_compareMinThree x y z = minThree x y z == minimum [x,y,z]

prop_minTr1, prop_minTr2 :: (Ord a) => a -> a -> a -> Bool
prop_minTr1 x y z = (x >= minThree x y z) && (y >= minThree x y z) && (z >= minThree x y z)
prop_minTr2 x y z = (x == minThree x y z ) || (y == minThree x y z) || (z == minThree x y z)

prop_maxTr1, prop_maxTr2 :: (Ord a) => a -> a -> a -> Bool
prop_maxTr1 x y z = (x <= maxThree x y z) && (y <= maxThree x y z) && (z <= maxThree x y z)
prop_maxTr2 x y z = (x == maxThree x y z ) || (y == maxThree x y z) || (z == maxThree x y z)