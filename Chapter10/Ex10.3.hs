import Prelude hiding (foldr1, foldr, concat, and, reverse, unzip, last, init)
import qualified Prelude
import Test.QuickCheck

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]     = x
foldr1 f (x:xs)  = x `f` (foldr1 f xs)
foldr1 _ _       = error "foldr1: Empty List"

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s (x:xs) = x `f` (foldr f s xs)
foldr _ s _      = s

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

and :: [Bool] -> Bool
and bs = foldr (&&) True bs

reverse :: [a] -> [a]
reverse = foldr snoc []

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

iSort :: (Ord a) => [a] -> [a]
iSort xs = foldr insA [] xs

insA :: (Ord a) => a -> [a] -> [a]
insA x (y:ys)
    | x <= y    = x : y : ys 
    | otherwise = y : insA x ys
insA x _        = [x]

-- 10.13)
square :: (Num a) => a -> a
square x = x*x

sumSquares :: (Enum a, Num a, Num b) => (a -> b) -> a -> b
sumSquares f n = foldr (+) 0 $ map f [1..n]

-- 10.14)
concatPairs :: (a,b) -> ([a],[b]) -> ([a], [b])
concatPairs (x,y) (xs,ys) = (x:xs, y:ys) 

unzip :: [(a,b)] -> ([a], [b])
unzip = foldr concatPairs ([], [])

-- last??
-- init??

-- 10.16)
mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs)

sing :: a -> [a]
sing x = [x]

-- mystery xs = foldr (++) [] (map sing xs)
--            = foldr (++) [] [sing x | x <- xs]
--            = foldr (++) [] [[x1], [x2], ..., [xn]]
--            = [x1] ++ [x2] ++ ... ++ [xn]
--            = [x1, x2, ..., xn]
--            = xs

prop_myst :: (Eq a) => [a] -> Bool
prop_myst xs = mystery xs == xs

-- 10.18)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x:xs)
    | p x       = x : filterFirst p xs
    | otherwise = xs
filterFirst _ _ = []

-- 10.19)
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p = reverse . filterFirst p . reverse

-- 10.20)
switchMap :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap f g = altTwoMaps f g True

altTwoMaps:: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
altTwoMaps f g b (x:xs)
    | b         = f x : altTwoMaps f g False xs
    | otherwise = g x : altTwoMaps f g True  xs
altTwoMaps _ _ _ _  = [] 

-- 10.21)
split :: [a] -> ([a], [a])
split xs = ([xs !! i | i <- [0,2..listSize]], [xs !! i | i <- [1,3..listSize]])
    where listSize = length xs - 1

merge :: ([a], [a]) -> [a]
merge (x:xs, y:ys) = x : y : merge (xs, ys)
merge (x:xs, _)    = x : merge (xs, [])
merge (_, y:ys)    = y : merge ([], ys)
merge _            = []

-- 10.22)
prop_splitMerge :: (Eq a) => [a] -> Bool
prop_splitMerge xs = merge (split xs) == xs
