import Prelude hiding (zip, take, drop, splitAt, zip3)
import qualified Prelude
import Test.QuickCheck

-- 1)
zip :: [a] -> [b] -> [(a,b)]
zip _ []          = []
zip [] _          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

prop_zip :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_zip xs ys = zip xs ys == Prelude.zip xs ys

-- 2)
take :: (Integral a) => a -> [b] -> [b]
take _ []       = []
take n (x:xs)
    | n <= 0    = []
    | otherwise = x : take (n-1) xs

prop_take :: (Integral a, Eq b) => a -> [b] -> Bool
prop_take n xs = take n xs == Prelude.take (fromEnum n) xs

-- 3)
qSort :: (Ord a) => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++
               [x]                         ++
               qSort [y | y <- xs, y > x]

-- 7.20)
drop :: (Integral a) => a -> [b] -> [b]
drop _ []       = []
drop n (x:xs)
    | n <= 0    = x:xs
    | otherwise = drop (n-1) xs

splitAt :: (Integral a) => a -> [b] -> ([b], [b])
splitAt n xs = (take n xs, drop n xs)

prop_drop ::(Integral a, Eq b) => a -> [b] -> Bool
prop_drop n xs = drop n xs == Prelude.drop (fromEnum n) xs

prop_splitAt :: (Integral a, Eq b) => a -> [b] -> Bool
prop_splitAt n xs = splitAt n xs == Prelude.splitAt (fromEnum n) xs

-- 7.21)
takeCheck :: (Integral a) => a -> [b] -> [b]
takeCheck 0 _   = []
takeCheck n (x:xs)
    | n < 0     = error "takeCheck : negative argument"
    | otherwise = x : takeCheck (n-1) xs
takeCheck _ []  = []

prop_takeCheck :: (Integral a, Eq b) => a -> [b] -> Bool
prop_takeCheck n xs
    | n < 0     = True
    | otherwise = takeCheck n xs == Prelude.take (fromEnum n) xs

-- 7.22)
zip' :: ([a],[b]) -> [(a,b)]
zip' (xs, ys) = zip xs ys

prop_ZUZ :: (Eq a, Eq b) => ([a], [b]) -> Bool
prop_ZUZ pairLists = (unzip . zip') pairLists == pairLists 

prop_UZZ :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_UZZ pairsList = (zip' . unzip) pairsList == pairsList

-- 7.23)
zip3R :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3R (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3R xs ys zs
zip3R _ _ _ = []

zip3 :: (Eq b) => [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = [(fst (zipXY !! i), snd (zipXY !! i), snd (zipYZ !! i)) | i <- [0..minLength-1]]
  where
    zipXY = zip xs ys
    zipYZ = zip ys zs
    minLength = minimum [length zipXY, length zipYZ]

prop_zip3R :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> Bool
prop_zip3R xs ys zs = zip3R xs ys zs == Prelude.zip3 xs ys zs

prop_zip3 :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> Bool
prop_zip3 xs ys zs = zip3 xs ys zs == Prelude.zip3 xs ys zs

-- 7.24)
qSortD :: (Ord a) => [a] -> [a]
qSortD []     = []
qSortD (x:xs) = qSortD [y | y <- xs, y >= x] ++
                [x]                          ++
                qSortD [y | y <- xs, y < x]

qSortU :: (Ord a) => [a] -> [a]
qSortU []     = []
qSortU (x:xs) = qSortU [y | y <- xs, y < x] ++
                [x]                         ++
                qSortU [y | y <- xs, y > x]

-- 7.25)
isSublist :: String -> String -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist (ch:chs) (st:str)
    | ch == st      = isSublist chs str
    | otherwise     = isSublist (ch:chs) str

isSubseq :: String -> String -> Bool
isSubseq [] _ = True
isSubseq _ [] = False
isSubseq (ch:chs) (st:str)
    | ch == st  = take chsLength chs == take chsLength str
    | otherwise = isSubseq (ch:chs) str
    where
        chsLength = length chs

data StrProp = Sublist | Subseq | Nap
               deriving (Eq, Show)

isWhat :: String -> String -> StrProp
isWhat str1 str2
    | isSubseq str1 str2  = Subseq
    | isSublist str1 str2 = Sublist
    | otherwise           = Nap

-- 7.26)
