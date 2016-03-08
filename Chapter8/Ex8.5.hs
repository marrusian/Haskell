import Prelude hiding (Word)
import Data.Char
import System.IO

-- 8)
copy :: IO ()
copy =
    do line <- getLine
       putStrLn line
       copy

-- 9)
copyN :: Integer -> IO ()
copyN n
    | n <= 0    = return ()
    | otherwise = do line <- getLine
                     putStrLn line
                     copyN (n-1)

-- 10)
copyEmpty :: IO ()
copyEmpty =
    do line <- getLine
       if line == ""
        then return ()
        else do putStrLn line
                copyEmpty

-- 11)
copyCount :: Integer -> IO ()
copyCount n =
    do line <- getLine
       if line == ""
        then putStrLn (show n ++ " lines copied.")
        else do putStrLn line
                copyCount (n+1)

-- BONUS)
copy_EOF :: IO ()
copy_EOF =
    do eof <- isEOF
       if eof
        then return ()
        else do line <- getLine
                putStrLn line
                copy_EOF

copyEOF :: IO ()
copyEOF =
    do hSetBuffering stdin LineBuffering
       copy_EOF
       hSetBuffering stdin NoBuffering

-- 8.14)
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

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

wc :: Int -> Int -> Int -> IO ()
wc lns wds chs =
    do line <- getLine
       if line == ""
        then do putStrLn ("Lines: " ++ show lns)
                putStrLn ("Words: " ++ show wds)
                putStrLn ("Chars: " ++ show chs)
        else do putStrLn line
                wc (lns+1) (wds + length (splitWords line)) (chs + length line)

-- 8.15)
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

isPalindromeIO :: IO ()
isPalindromeIO =
    do putStr "Insert a string for palindrome test: "
       line <- getLine
       putStr line
       if isPalin line
         then    putStrLn " is a palindrome!"
         else do putStrLn " is NOT a palindrome!"
       isPalindromeIO

-- 8.16)
isPalindromeIO2 :: IO ()
isPalindromeIO2 =
    do putStr "Insert a string for palindrome test: "
       line <- getLine
       if line == ""
        then return ()
        else do putStr line
                if isPalin line
                 then    putStrLn " is a palindrome!"
                 else do putStrLn " is NOT a palindrome!"
                isPalindromeIO2

-- 8.17)
getInt :: IO Integer
getInt =
    do line <- getLine
       return (read line :: Integer)

sumIntsIO :: Integer -> IO ()
sumIntsIO sum =
    do val <- getInt
       if val == 0
        then putStrLn ("The sum is: " ++ show sum)
        else sumIntsIO (sum+val)

-- 8.18)
insA :: (Ord a) => a -> [a] -> [a]
insA x []     = [x]
insA x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y : insA x ys

sortIntsIO :: [Integer] -> IO ()
sortIntsIO xs =
    do val <- getInt
       if val == 0
        then putStrLn ("Sorted integers: " ++ show xs)
        else sortIntsIO (insA val xs)

-- 8.19)
--   Because Haskell only supports single assignment, the second "line" variable
-- is completely different from the first "line" variable, so that the
-- (line == "") condition will be true only when the first line read is empty.
--   If the first line read is not empty, the program will loop forever.

copy819 :: IO ()
copy819 =
    do line <- getLine
       let whileCopy = do if line == ""
                           then return ()
                           else do putStrLn line
                                   line <- getLine
                                   whileCopy
       whileCopy
