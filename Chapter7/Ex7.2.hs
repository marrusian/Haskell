import Prelude hiding (head, tail, null)
import Data.Char

head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "tail: empty list"

null :: [a] -> Bool
null [] = True
null _  = False

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

firstDigit :: String -> Char
firstDigit st
    = case (digits st) of
        []    -> '\0'
        (x:_) -> x

-- 7.1)
firstInteger :: (Integral a) => [a] -> a
firstInteger (x:xs) = x+1
firstInteger []     = 0

-- 7.2)
func :: (Integral a) => [a] -> a
func (x:y:xs) = x+y
func [x]      = x
func []       = 0

-- 7.3)
firstInteger' :: (Integral a) => [a] -> a
firstInteger' xs = sum (take 1 xs)

func' :: (Integral a) => [a] -> a
func' xs = sum (take 2 xs)

-- 7.4)
firstDigitFast :: String -> Char
firstDigitFast ""       = '\0'
firstDigitFast (ch:str)
    | isDigit ch = ch
    | otherwise  = firstDigitFast str

firstDigitAlt :: String -> Char
firstDigitAlt st = if null digs then '\0' else head digs
    where
        digs = digits st

