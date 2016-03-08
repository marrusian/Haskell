import Prelude hiding (putStrLn)
import Data.Char

-- 1)
putStrLn :: String -> IO ()
putStrLn str = do putStr str
                  putStr "\n"

-- 2)
put4times :: String -> IO ()
put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str

-- 3)
read2lines :: IO ()
read2lines = do getLine
                getLine
                putStrLn "Two lines read."

-- 4)
getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

-- 5)
reverse2lines :: IO ()
reverse2lines =
    do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)

-- 6)
reverse2lines' :: IO ()
reverse2lines'
    = do line1 <- getLine
         line2 <- getLine
         let rev1 = reverse line1
         let rev2 = reverse line2
         putStrLn rev1
         putStrLn rev2

-- 7)
inputInt :: IO Integer
inputInt =
    do line <- getLine
       return (read line :: Integer)

-- 8.10)
isPal :: String -> Bool
isPal [x]      = True
isPal (ch:str)
    | ch == last str = isPal (init str)
    | otherwise      = False
isPal []       = True

isPalindromeIO :: IO ()
isPalindromeIO =
    do putStr "Insert a string for palindrome test: "
       line <- getLine
       putStr line
       if isPal line
         then putStrLn " is a palindrome!"
         else putStrLn " is NOT a palindrome!"

-- 8.11)
sum2intIO :: IO ()
sum2intIO =
    do putStr "Insert first integer: "
       n <- inputInt
       putStr "Insert second integer: "
       m <- inputInt
       putStrLn (show n ++ " + " ++ show m ++ " = " ++ show (n+m))

-- 8.12)
putNtimes :: Integer -> String -> IO ()
putNtimes n str
    | n > 0     = do putStrLn str
                     putNtimes (n-1) str
    | otherwise = return ()

-- 8.13)
promptNthIntIO :: Integer -> IO ()
promptNthIntIO i
    | i <= 0    = return ()
    | i == 1    = putStr "Insert 1st integer: "
    | i == 2    = putStr "Insert 2nd integer: "
    | otherwise = putStr ("Insert " ++ show i ++ "th integer: ")

sumNintIO :: IO ()
sumNintIO =
    do n   <- inputInt
       sum <- sumInt n 1
       putStrLn ("The sum is: " ++ show sum)
    where
        sumInt :: Integer -> Integer -> IO Integer
        sumInt n i
            | i <= n    = do promptNthIntIO i
                             val  <- inputInt
                             valR <- sumInt n (i+1)
                             return (val + valR)
            | otherwise = return 0

sumNintIO2 :: IO ()
sumNintIO2 =
    do n   <- inputInt
       seq <- sequence [promptNthIntIO i >> inputInt | i <- [1..n]]
       putStrLn ("The sum is: " ++ show (sum seq))
