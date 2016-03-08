import Prelude hiding (Word, getLine)
import Data.Char

whitespace :: [Char]
whitespace = ['\n', '\t', ' ']

getWord :: String -> String
getWord []     = []
getWord (x:xs)
    | x `elem` whitespace = []
    | otherwise           = x : getWord xs 

dropWord :: String -> String
dropWord []     = []
dropWord (x:xs)
    | x `elem` whitespace = (x:xs)
    | otherwise           = dropWord xs

dropSpace :: String -> String
dropSpace []     = []
dropSpace (x:xs)
    | x `elem` whitespace = dropSpace xs
    | otherwise           = (x:xs)

type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

type Line = [Word]

getLine :: (Integral a) => a -> [Word] -> Line
getLine _ [] = []
getLine n (w:ws)
    | wordLength <= n = w : getLine (n - wordLength) ws
    | otherwise       = []
    where
        wordLength = toEnum (length w + 1)

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine lineLength ws : splitLines (dropLine lineLength ws)
    where
        lineLength = 37

fill :: String -> [Line]
fill = splitLines . splitWords

-- 7.27)
dropLine :: (Integral a) => a -> [Word] -> Line
dropLine _ [] = []
dropLine n (w:ws)
    | wordLength <= n = dropLine (n - wordLength) ws
    | otherwise       = (w:ws)
    where
        wordLength = toEnum (length w + 1)

-- 7.28)
joinLine :: Line -> String
joinLine ln = init (concat [w ++ " "| w <- ln])

-- 7.29)
joinLines :: [Line] -> String
joinLines ls = concat [joinLine line ++ "\n" | line <- ls]

-- 7.32)
wc :: String -> (Int, Int, Int)
wc txt = (length txt, length words, numLines)
    where
        words    = splitWords txt
        numLines = sum [1 | ch <- txt, ch == '\n']
        -- numLines = sum [1 | w <- words, last w == '\n']

wcFormat :: String -> (Int, Int, Int)
wcFormat txt = wc (joinLines (fill txt))

-- 7.33)
isPal :: String -> Bool
isPal [x]      = True
isPal (ch:str)
    | ch == last str = isPal (init str)
    | otherwise      = False
isPal []       = True

ignPunctCase :: String -> String
ignPunctCase str = [toLower ch | ch <- str, not (isPunctuation ch) && not (isSpace ch)]

isPalin :: String -> Bool
isPalin str = isPal (ignPunctCase str)

-- 7.34)

type Index = Int

isSubseq :: String -> String -> (Bool, Index)
isSubseq str1 str2 = isSubsq str1 str2 0

isSubsq :: String -> String -> Index -> (Bool, Index)
isSubsq [] _ i = (True,i)
isSubsq _ [] i = (False,i)
isSubsq (ch:chs) (st:str) i
    | ch == st  = (take chsLength chs == take chsLength str, i)
    | otherwise = isSubsq (ch:chs) str (i+1)
    where
        chsLength = length chs

replace :: String -> String -> String -> String
replace oldSub newSub str
    | fst subseqPair = fst splitStr ++ newSub ++ dropOldSub
    | otherwise      = str
    where
        subseqPair = isSubseq oldSub str
        splitStr   = splitAt (snd subseqPair) str
        dropOldSub = drop (length oldSub) (snd splitStr)
