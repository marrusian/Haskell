import Prelude hiding (map, filter, zipWith, length)
import qualified Prelude
import Test.QuickCheck

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map _ _      = []

mapLC :: (a -> b) -> [a] -> [b]
mapLC f xs = [f x | x <- xs]

double :: (Num a) => a -> a
double = (2*)

doubleAll :: (Num a) => [a] -> [a]
doubleAll = map double

doubleAllR :: (Num a) => [a] -> [a]
doubleAllR (x:xs) = 2*x : doubleAllR xs
doubleAllR   _    = []

doubleAllLC :: (Num a) => [a] -> [a]
doubleAllLC xs = [2*x | x <- xs]

convertChrs :: [Char] -> [Int]
convertChrs = map fromEnum

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise =     filter p xs
filter _ _          = []

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p xs = [x | x <- xs, p x]

isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')

digits :: [Char] -> [Char]
digits = filter isDigit

isEven :: (Integral a) => a -> Bool
isEven x = x `mod` 2 == 0

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _           = []

-- 10.1)
{-
doubleAllLC [2,1,7] = [2*x | x <- [2,1,7]]
    [2*2 | x <- [1,7]]
    [2*2, 2*1 | x <- [7]]
    [2*2, 2*1, 2*7 | x <- []]
    = [4,2,14]

doubleAllR [2,1,7] = 
    2*2 : doubleAllR [1,7]
    2*2 : 2*1 : doubleAllR [7]
    2*2 : 2*1 : 2*7 : doubleAllR []
    2*2 : 2*1 : 2*7 : []
    = [4,2,14]

doubleAll [2,1,7] = map double [2,1,7]
    double 2 : map double [1,7]
    double 2 : double 1 : map double [7]
    double 2 : double 1 : double 7 : map double []
    double 2 : double 1 : double 7 : []
    4 : 1 : 7 : []
    = [4,1,7]
-}

-- 10.2)
length :: [a] -> Int
length = sum . map (const 1)

propLength :: [a] -> Bool
propLength xs = length xs == Prelude.length xs 

-- 10.3)
greaterOne :: (Ord a, Num a) => a -> Bool
greaterOne = (> 1)

addOne :: (Num a) => a -> a
addOne = (+ 1)

addUp :: (Ord a, Num a) => [a] -> [a]
addUp = filter greaterOne . map addOne

-- Filtering BEFORE mapping
-- Condition: x+1 > 1 => x > 0

greaterZero :: (Ord a, Num a) => a -> Bool
greaterZero = (> 0)

addUp' :: (Ord a, Num a) => [a] -> [a]
addUp' = map addOne . filter greaterZero

prop_addUp :: (Ord a, Num a) => [a] -> Bool
prop_addUp xs = addUp xs == addUp' xs

-- 10.4)
addTwoAll :: (Num a) => [a] -> [a]
addTwoAll = map addOne . map addOne

prop_addTwoAll :: (Num a, Eq a) => [a] -> Bool
prop_addTwoAll xs = addTwoAll xs == [x+2 | x <- xs]

-- map f (map g xs) == [(f.g) x | x <- xs]
map2 :: (b -> c) -> (a -> b) -> [a] -> [c]
map2 f g = map f . map g

map2LC :: (b -> c) -> (a -> b) -> [a] -> [c]
map2LC f g xs = [f.g $ x | x <- xs]

map2R :: (b -> c) -> (a -> b) -> [a] -> [c]
map2R f g (x:xs) = (f.g) x : map2R f g xs
map2R _ _ _      = []

-- 10.5)
lessTen :: (Num a, Ord a) => a -> Bool
lessTen = (<10)

{-
 filter greaterOne (filter lessTen ns) = [x | x <- [y | y<-xs, y<10], x>1]
                                       = [x | x <- xs, x<10, x>1]
                                       = [x | x <- xs, x>1 && x<10]

    Observation: [x | x <- xs, x>1 && x<10] is MORE EFFICIENT than
                 [x | x <- [y | y<-xs, y<10], x>1]
-}

betweenOneAndTen :: (Num a, Ord a) => [a] -> [a]
betweenOneAndTen = filter greaterOne . filter lessTen

filter2 :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter2 p q = filter p . filter q

filter2LC :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter2LC p q xs = [x | x <- xs, q x && p x]

filter2R :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter2R p q (x:xs)
    | q x && p x = x : filter2R p q xs
    | otherwise  =     filter2R p q xs
filter2R _ _ _   = []

-- 10.6)
sq :: (Num a) => a -> a
sq x = x*x

square :: (Num a) => [a] -> [a]
square = map sq

sumSquare :: (Num a) => [a] -> a
sumSquare = sum . square

isPos :: (Num a, Ord a) => a -> Bool
isPos = (> 0)

isPositive :: (Num a, Ord a) => [a] -> Bool
isPositive = and . map isPos

-- 10.7)
minFunc :: (Enum a, Num a, Ord a, Ord b) => (a -> b) -> a -> b
minFunc f n
    | n >= 0    = minimum . map f $ [0..n]
    | otherwise = error "minFunc: Negative second argument"

isEq :: (Eq a) => a -> a -> Bool
isEq x y = x == y

isEqual :: (Enum a, Num a, Ord a) => (a -> a) -> a -> Bool
isEqual f n
    | n >= 0    = and . map (fstElem `isEq`) $ mappedList
    | otherwise = error "isEqual: Negative second argument"
    where mappedList = map f [0..n]
          fstElem    = head mappedList

isPositive' :: (Enum a, Num a, Num b, Ord b) => (a -> b) -> a -> Bool
isPositive' f n = and . map ((>0) . f) $ [0..n]

isAscending :: (Ord a) => [a] -> Bool
isAscending [x] = True
isAscending (x:y:xs)
    | x <= y    = isAscending xs
    | otherwise = False
isAscending _   = True

isIncreasing :: (Enum a, Num a, Ord b) => (a -> b) -> a -> Bool
isIncreasing f n = isAscending $ map f [0..n]

-- 10.8)
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

-- 10.9)
iter :: (Integral a) => a -> (b -> b) -> b -> b
iter 0 _ x      = x
iter n f x
    | n > 0     = f $ iter (n-1) f x
    | otherwise = error "iter: Negative first argument"

-- 10.10)
twoExp :: (Integral a) => a -> Integer
twoExp n = iter n double 1

-- 10.11)
prop_filter1 :: (Eq a) => (a -> Bool) -> [a] -> Bool
prop_filter1 p xs = filter p xs == [x | x <- xs, p x]

prop_filter2 :: (a -> Bool) -> [a] -> Bool
prop_filter2 p xs = length (filter p xs) <= length xs

prop_filter3 :: (Eq a) => (a -> Bool) -> [a] -> Bool
prop_filter3 p xs = filter p xs /= xs

-- 10.12)
-- map g (map f xs) == xs
