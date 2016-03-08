import Data.Char

-- 5.18)
doubleAll :: (Integral a) => [a] -> [a]
doubleAll xs = [2*n | n <- xs]

-- 5.19)
capitalize :: String -> String
capitalize str = [toUpper ch | ch <- str]

capitalizeLetters :: String -> String
capitalizeLetters str = [toUpper ch | ch <- str, isAlpha ch]

-- 5.20)
divisors :: (Integral a) => a -> [a]
divisors n
    | n > 0     = [d | d <- [1.. n `div` 2], n `mod` d == 0] ++ [n]
    | otherwise = []

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n > 0 && length (divisors n) == 2 = True
    | otherwise                         = False

isPrime' :: (Integral a) => a -> Bool
isPrime' n
    | n > 0     = [d | d <- [2..floor (sqrt (fromIntegral n))], n `mod` d == 0] == []
    | otherwise = False

-- 5.21)
matches :: (Integral a) => a -> [a] -> [a]
matches x xs = [y | y <- xs, y == x]

elem' :: (Integral a) => a -> [a] -> Bool
elem' x xs = matches x xs /= []

elem'' :: (Integral a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' n (x:xs)
    | x == n    = True
    | otherwise = elem'' n xs

-- 5.22)
onSeparateLines :: [String] -> String
onSeparateLines []         = []
onSeparateLines (str:strs) = (str ++ "\n") ++ onSeparateLines strs

-- 5.23)
duplicate :: (Integral a) => String -> a -> String
duplicate "" _ = ""
duplicate str n
    | n > 0     = str ++ duplicate str (n-1)
    | otherwise = ""

-- 5.24)  && 5.25) [left pad, lpad]
pushRight :: String -> Int -> String
pushRight str lineLength
    | lineLength > length str = " " ++ pushRight str (lineLength-1)
    | otherwise               = str 

-- 5.26)
fibStep :: (Integral a) => (a, a) -> (a, a)
fibStep (u,v) = (v, u+v)

fibPair :: (Integral a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n-1))

fastFib :: (Integral a) => a -> a
fastFib = fst.fibPair

fibTable :: Integer -> String
fibTable n
    | n >= 0    = "n \t fib n\n" ++ makeFibTable n 0
    | otherwise = error "fibTable: Function is defined only for natural numbers"
    where
        makeFibTable :: Integer -> Integer -> String
        makeFibTable n m
            | n >= m    = show m ++ " \t " ++ show (fastFib m) ++ "\n" ++ makeFibTable n (m+1)
            | otherwise = ""

