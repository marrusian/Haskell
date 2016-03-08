import Test.QuickCheck
import Test.HUnit

-- Ex1)
minmax :: (Integral a) => a -> a -> (a, a)
minmax x y
    | x >= y    = (y, x)
    | otherwise = (x, y)

addPair :: (Integral a) => (a, a) -> a
addPair (x, y) = x+y

addPair' :: (Integral a) => (a, a) -> a
addPair' p = fst p + snd p

shift :: (Integral a) => ((a, a), a) -> (a, (a, a))
shift ((x, y), z) = (x, (y, z))

type ShopItem = (String, Int)
type Basket   = [ShopItem]

name  :: ShopItem -> String
price :: ShopItem -> Int

name  (n, p) = n
price (n, p) = p

-- Ex3)
fibStep :: (Integral a) => (a, a) -> (a, a)
fibStep (u,v) = (v, u+v)

fibPair :: (Integral a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n-1))

fastFib :: (Integral a) => a -> a
fastFib = fst.fibPair

-- 5.1)
maxOccurs :: (Integral a) => a -> a -> (a, a)
maxOccurs x y = (maxi, occ maxi)
    where
        maxi = max x y
        occ n
            | n == x && n == y = 2
            | otherwise        = 1

maxThreeOccurs :: (Integral a) => a -> a -> a -> (a, a)
maxThreeOccurs x y z = (maxi, occ maxi)
    where
        maxi = max (max x y) z
        occ n
            | n == x && n == y && n == z = 3
            | n == x && n == y ||
              n == x && n == z ||
              n == y && n == z           = 2
            | otherwise                  = 1

-- 5.2)
minThree :: (Ord a) => a -> a -> a -> a
minThree x y z
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

maxThree :: (Ord a) => a -> a -> a -> a
maxThree x y z
    | x >= y && x >= z = x
    | y >= z           = y
    | otherwise        = z 

weakAscendingOrder :: (Ord a) => a -> a -> a -> Bool
weakAscendingOrder m n p
    | (m <= n) && (n <= p) = True
    | otherwise            = False

between :: (Ord a) => a -> a -> a -> Bool
between m n p
    | weakAscendingOrder m n p   = True
    | otherwise                  = False

middleThree :: (Integral a) => a -> a -> a -> a
middleThree x y z
    | between y x z || between z x y = x
    | between x y z || between z y x = y
    | otherwise                      = z

orderTriple :: (Integral a) => (a, a, a) -> (a, a, a)
orderTriple (x, y, z) = (minThree x y z, middleThree x y z, maxThree x y z)

-- 5.3)
whereCrossesOX :: (Floating a, Eq a) => (a, a) -> (a, a) -> (a, Bool)
whereCrossesOX (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = error "This function takes two different points on the line"
    | x1 == x2             = (x1,  True)         -- or x2, doesn't matter (it's a vertical line)
    | y1 == y2 && y1 /= 0  = (0/0, False)        -- Horizontal line which differs from OX
    | y1 == y2 && y1 == 0  = (0/0, True)         -- Horizontal line which coincides with OX
    | otherwise            = ((-b)/slope, True)  -- y = m*x + b => x = (y-b)/m
    where
        slope = (y2-y1)/(x2-x1)
        b     = y1-slope*x1

-- 5.4)
-- 5.1) Test Data
testMO1 = TestCase (assertEqual "for: maxOccurs 1 1" (1, 2) (maxOccurs 1 1))
testMO2 = TestCase (assertEqual "for: maxOccurs 1 0" (1, 1) (maxOccurs 1 0))

testsMO = TestList [testMO1, testMO2]

prop_MO1 :: (Integral a) => a -> a -> Bool
prop_MO1 x y = maxOccurs x y == maxOccurs y x

prop_MO2 :: (Integral a) => a -> a -> Bool
prop_MO2 x y = fst (maxOccurs x y) == x || fst (maxOccurs x y) == y

prop_MO3 :: (Integral a) => a -> a -> Bool
prop_MO3 x y = fst (maxOccurs x y) == max x y

-- 5.2) Test Data
testOT1 = TestCase (assertEqual "for: orderTriple (0,0,0)" (0,0,0) (orderTriple (0,0,0)))
testOT2 = TestCase (assertEqual "for: orderTriple (1,0,0)" (0,0,1) (orderTriple (1,0,0)))
testOT3 = TestCase (assertEqual "for: orderTriple (0,1,0)" (0,0,1) (orderTriple (0,1,0)))
testOT4 = TestCase (assertEqual "for: orderTriple (0,0,1)" (0,0,1) (orderTriple (0,0,1)))
testOT5 = TestCase (assertEqual "for: orderTriple (1,1,0)" (0,1,1) (orderTriple (1,1,0)))
testOT6 = TestCase (assertEqual "for: orderTriple (1,0,1)" (0,1,1) (orderTriple (1,0,1)))
testOT7 = TestCase (assertEqual "for: orderTriple (0,1,1)" (0,1,1) (orderTriple (0,1,1)))
testOT8 = TestCase (assertEqual "for: orderTriple (1,1,1)" (1,1,1) (orderTriple (1,1,1)))

testsOT = TestList [testOT1, testOT2, testOT3, testOT4, testOT5, testOT6, testOT7, testOT8]

prop_OT1 :: (Integral a) => (a, a, a) -> Bool
prop_OT1 (x, y, z) = orderTriple (x,y,z) == (x,y,z) || orderTriple (x,y,z) == (x,z,y) ||
                     orderTriple (x,y,z) == (y,x,z) || orderTriple (x,y,z) == (y,z,x) ||
                     orderTriple (x,y,z) == (z,x,y) || orderTriple (x,y,z) == (z,y,x)

prop_OT2 :: (Integral a) => (a, a, a) -> Bool
prop_OT2 (x, y, z) = orderTriple (x,y,z) == orderTriple (x,z,y) &&
                     orderTriple (x,z,y) == orderTriple (y,x,z) &&
                     orderTriple (y,x,z) == orderTriple (y,z,x) &&
                     orderTriple (y,z,x) == orderTriple (z,x,y) &&
                     orderTriple (z,x,y) == orderTriple (z,y,x) &&
                     orderTriple (z,y,x) == orderTriple (x,y,z)

fst' :: (a,b,c) -> a
fst' (x,y,z) = x

snd' :: (a,b,c) -> b
snd' (x,y,z) = y

trd' :: (a, b, c) -> c
trd' (x, y, z) = z

prop_OT3 :: (Integral a) => (a, a, a) -> Bool
prop_OT3 (x, y, z) = trd' (orderTriple (x, y, z)) == max (max x y) z

prop_OT4 :: (Integral a) => (a, a, a) -> Bool
prop_OT4 (x, y, z) = fst' (orderTriple (x, y, z)) == min (min x y) z

prop_OT5 :: (Integral a) => (a, a, a) -> Bool
prop_OT5 (x, y, z) = fst' (orderTriple (x, y, z)) <= snd' (orderTriple (x, y, z)) &&
                     snd' (orderTriple (x, y, z)) <= trd' (orderTriple (x, y, z))

-- 5.3) Test Data [to be continued]

