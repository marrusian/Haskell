sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: (Num b) => [a] -> b
length'' xs = sum [1 | x <- xs]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a < b     = LT
    | a == b    = EQ
    | otherwise = GT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where ((f:_), (l:_)) = (firstName, lastName)

calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height^2

calcBMIs' :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs' xs = [bmi | (w,h) <- xs, let bmi = w / h^2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = let sideArea = 2*pi * r * h
                   topArea  = pi * r^2
               in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []  = "empty"
          what [x] = "a singleton list"
          what xs  = "a longer list"

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []  = []
zip' [] _  = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys)
	| x == y    = True
	| otherwise = x `elem'` ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
		let smallerSorted = quicksort [a | a <- xs, a <= x]
		    biggerSorted  = quicksort [a | a <- xs, a > x]
		in  smallerSorted ++ [x] ++ biggerSorted


ispal :: String -> Bool
ispal []  = True
ispal [x] = True
ispal (x:xs)
	| x == last xs = ispal (init xs)
	| otherwise = False

maxC :: (Ord a) => a -> a -> a
maxC x y
    | x > y = x
    | otherwise = y

maxP :: (Ord a) => (a,a) -> a
maxP (x,y)
    | x > y = x
    | otherwise = y

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

--isUpper :: Char -> Bool
--isUpper = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [10^5, 10^5-1 ..])
    where p x = x `mod` 3829 == 0

isPrime :: (Integral a, Eq a) => a -> Bool
isPrime x
    | length [y | y <- [2 .. floor (sqrt (fromIntegral x))], x `mod` y == 0] == 0 = True
    | otherwise = False

primes :: (Integral a) => [a]
primes = [x | x <- [2..], isPrime x]

factor :: (Integral a, Eq a) => a -> [a]
factor 0 = [0]
factor x
    | x>0       = [y | y <- [1.. x `div` 2], x `mod` y == 0] ++ [x]
    | otherwise = map negate (factor (-x))

twoExp :: (Integral a) => a -> a
twoExp 0 = 1
twoExp n
    | n < 0          = error "twoExp: negative exponent"
    | n `mod` 2 == 0 = (twoExp (n `div` 2))^2
    | n `mod` 2 == 1 = 2*(twoExp ((n-1) `div` 2))^2


