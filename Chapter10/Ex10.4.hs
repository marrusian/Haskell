import Prelude hiding (takeWhile, dropWhile, getLine)
import qualified Prelude
import Data.Char

whitespace = [' ', '\n', '\t']

getWord :: String -> String
getWord (x:xs)
    | x `elem` whitespace = []
    | otherwise           = x : getWord xs
getWord _ = []

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p (x:xs)
    | p x       = []
    | otherwise = x : getUntil p xs
getUntil _ _ = []

getWord' :: String -> String
getWord' = getUntil p
    where p :: Char -> Bool
          p x = x `elem` whitespace

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []
takeWhile _ _ = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = x : xs
dropWhile _ _ = []

-- 10.24)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p (x:xs)
    | p x       = xs
    | otherwise = dropUntil p xs
dropUntil _ _  = []

-- 10.25)
dropSpace :: String -> String
dropSpace = dropUntil (not . isSpace)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = getUntil (not . p)

-- 10.26)
splitLines :: String -> [String]
splitLines [] = []
splitLines ls = getUntil isDot ls : splitLines (dropUntil isDot ls)
    where isDot = ('.' ==)

-- 10.27)
getLists :: Int -> [[a]] -> [[a]]
getLists n (x:xs)
    | lstLgth <= n = x : getLists (n - lstLgth) xs
    | otherwise    = []
    where lstLgth = length x
getLists _ _ = []

-- 10.28)
-- getLine()
getLists' :: (Integral i) => ([a] -> i -> Bool) -> i -> ([a] -> i -> i) -> [[a]] -> [[a]]
getLists' pn n expn (x:xs)
    | pn x n    = x : getLists' pn (expn x n) expn xs
    | otherwise = []
getLists' _ _ _ _ = []

getLine :: Int -> [String] -> [String]
getLine n sts = getLists' pn n expn sts
    where pn   x n = (length x <= n)
          expn x n = (n - length x)

-- dropLine()
dropLists :: Int -> [[a]] -> [[a]]
dropLists _ [] = []
dropLists n (w:ws)
    | wordLength <= n = dropLists (n - wordLength) ws
    | otherwise       = (w:ws)
    where
        wordLength = (length w + 1)

dropLists' :: (Integral i) => ([a] -> i -> Bool) -> i -> ([a] -> i -> i) -> [[a]] -> [[a]]
dropLists' pn n expn (x:xs)
    | pn x n    = dropLists' pn (expn x n) expn xs
    | otherwise = (x:xs)
dropLists' _ _ _ _ = []

dropLine :: Int -> [String] -> [String]
dropLine n sts = dropLists' pn n expn sts
    where pn   x n = length x <= n
          expn x n = n - length x

-- splitLines()
splitLines' :: (Int -> [[a]] -> [[a]]) -> (Int -> [[a]] -> [[a]]) -> [[a]] -> [[[a]]]
splitLines' _ _ [] = []
splitLines' f g ws = f lineLength ws : splitLines' f g (g lineLength ws)
    where
        lineLength = 37
