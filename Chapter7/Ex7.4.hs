import Prelude hiding (concat, (++), elem, reverse, unzip, maximum, minimum)
import qualified Prelude
import Test.QuickCheck

-- 1)
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

prop_concat :: (Eq a) => [[a]] -> Bool
prop_concat xs = concat xs == Prelude.concat xs

-- 2)
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x:(xs++ys)

prop_pp :: (Eq a) => [a] -> [a] -> Bool
prop_pp xs ys = xs ++ ys == xs Prelude.++ ys

-- 3)
elem :: (Eq a) => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = (x == y) || (elem x ys)

prop_elem :: (Eq a) => a -> [a] -> Bool
prop_elem x xs = x `elem` xs == Prelude.elem x xs

-- 4)
doubleAll :: (Num a) => [a] -> [a]
doubleAll xs = [2*x | x <- xs]

doubleAllR :: (Num a) => [a] -> [a]
doubleAllR []     = []
doubleAllR (x:xs) = 2*x:doubleAllR xs

prop_doubleAll :: (Eq a, Num a) => [a] -> Bool
prop_doubleAll xs = doubleAll xs == doubleAllR xs

-- 5)
isEven :: (Integral a) => a -> Bool
isEven x = x `mod` 2 == 0

isOdd :: (Integral a) => a -> Bool
isOdd = not . isEven

selectEven :: (Integral a) => [a] -> [a]
selectEven xs = [x | x <- xs, isEven x]

selectEvenR :: (Integral a) => [a] -> [a]
selectEvenR []     = []
selectEvenR (x:xs)
    | isEven x  = x:selectEvenR xs
    | otherwise =   selectEvenR xs

prop_selectEven :: (Eq a, Integral a) => [a] -> Bool
prop_selectEven xs = selectEven xs == selectEvenR xs

-- 6)
qSort :: (Ord a) => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]

iSort :: (Ord a) => (a -> [a] -> [a]) -> [a] -> [a]
iSort _ []     = []
iSort ins (x:xs) = ins x (iSort ins xs)

insA :: (Ord a) => a -> [a] -> [a]
insA x []     = [x]
insA x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y : insA x ys

prop_sort :: (Eq a, Ord a) => [a] -> Bool
prop_sort xs = iSort insA xs == qSort xs

-- 7.8)
fromBool :: (Num a) => Bool -> a
fromBool b
    | b         = 1
    | otherwise = 0

elemNumR :: (Eq a, Num b) => a -> [a] -> b
elemNumR _ []     = 0
elemNumR x (y:ys) = fromBool (x==y) + elemNumR x ys

elemNum :: (Eq a, Num b) => a -> [a] -> b
elemNum x xs = sum [1 | y <- xs, y == x]

prop_elemNum :: (Eq a, Num b) => a -> [a] -> Bool
prop_elemNum x xs = elemNum x xs == elemNumR x xs

-- 7.9)
unique :: (Eq a, Num a) => [a] -> [a]
unique xs = [x | x <- xs, elemNum x xs == 1]

uniqueR :: (Eq a, Num a) => [a] -> [a]
uniqueR []     = []
uniqueR (x:xs)
    | elemNum x xs == 0 = x : uniqueR xs
    | otherwise         =     uniqueR [y | y <- xs, x /= y]

prop_unique :: (Eq a, Num a) => [a] -> Bool
prop_unique xs = unique xs == uniqueR xs

-- 7.10)
prop_UEN :: (Eq a, Num a) => [a] -> Bool
prop_UEN xs = and [elemNum x uniqueList == 1 | x <- uniqueList]
    where
        uniqueList = unique xs

-- 7.11)
reverse :: [a] -> [a]
reverse [] = []
reverse xs = last xs : reverse (init xs)

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse xs = reverse xs == Prelude.reverse xs

unzip :: [(a,b)] -> ([a], [b])
unzip xs = ([fst x | x <- xs], [snd x | x <- xs])

unzipR :: [(a,b)] -> ([a], [b])
unzipR [] = ([], [])
unzipR ((x,y):ps) = (x : fst unzippedList, y : snd unzippedList)
    where
        unzippedList = unzipR ps

prop_unzip :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_unzip xs = unzip xs == Prelude.unzip xs

prop_unzipR :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_unzipR xs = unzipR xs == Prelude.unzip xs

-- 7.12)
minimax :: (Ord a) => [a] -> (a,a)
minimax [] = error "minimax: Empty list"
minimax xs = (head sortedList, last sortedList)
    where
        sortedList = iSort insA xs

minimaxR :: (Ord a) => [a] -> (a,a)
minimaxR []       = error "minimaxR: Empty List"
minimaxR [x]      = (x, x)
minimaxR [x,y]    = if x>y then (y,x) else (x,y)
minimaxR (x:y:xs)
    | y < x     = (y, snd mima)
    | otherwise = (fst mima, y)
    where
        mima = minimaxR xs

maximum :: (Ord a) => [a] -> a
maximum []     = error "maximum: Empty list"
maximum [x]    = x
maximum (x:xs)
    | x > maxx  = x
    | otherwise = maxx
    where
        maxx = maximum xs

minimum :: (Ord a) => [a] -> a
minimum []  = error "minimum: Empty list"
minimum [x] = x
minimum (x:xs)
    | x < minn   = x
    | otherwise  = minn
    where
        minn = minimum xs

prop_minimax :: (Ord a) => [a] -> Bool
prop_minimax [] = True
prop_minimax xs = minimax xs == (Prelude.minimum xs, Prelude.maximum xs)

prop_minimaxR :: (Ord a) => [a] -> Bool
prop_minimaxR [] = True
prop_minimaxR xs = minimaxR xs == (Prelude.minimum xs, Prelude.maximum xs)

prop_minmaxEquiv :: (Ord a) => [a] -> Bool
prop_minmaxEquiv [] = True
prop_minmaxEquiv xs = minimax xs == minimaxR xs

prop_maximum :: (Ord a) => [a] -> Bool
prop_maximum [] = True
prop_maximum xs = maximum xs == Prelude.maximum xs

prop_minimum :: (Ord a) => [a] -> Bool
prop_minimum [] = True
prop_minimum xs = minimum xs == Prelude.minimum xs

-- 7.13) TO BE MADE

-- 7.14)
isSorted :: (Ord a) => [a] -> Bool
isSorted []     = True
isSorted [x]    = True
isSorted (x:xs) = x <= head xs && isSorted xs 

prop_iSort :: (Ord a) => [a] -> Bool
prop_iSort xs = isSorted (iSort insA xs)

prop_insA :: (Ord a) => a -> [a] -> Bool
prop_insA x xs = isSorted (takeWhile (< x) (insA x xs))

-- 7.15) ??????

-- 7.16)
insD :: (Ord a) => a -> [a] -> [a]
insD x [] = [x]
insD x (y:ys)
    | x > y     = x:(y:ys)
    | otherwise = y : insD x ys

insAU :: (Ord a) => a -> [a] -> [a]
insAU x [] = [x]
insAU x (y:ys)
    | x == y    = y:ys
    | x < y     = x:(y:ys)
    | otherwise = y : insAU x ys

-- 7.17)
isSortedD :: (Ord a) => [a] -> Bool
isSortedD []     = True
isSortedD [x]    = True
isSortedD (x:xs) = x >= head xs && isSortedD xs

isSortedAU :: (Ord a) => [a] -> Bool
isSortedAU = isSorted

-- 7.18) TO BE MADE

-- 7.19)
--   This behaviour is already defined, because
-- of the built-in (<) overloading on pairs.
