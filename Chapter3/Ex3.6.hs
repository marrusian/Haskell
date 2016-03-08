import Test.QuickCheck

-- 3.20)
averageThree :: Integer -> Integer -> Integer -> Float
averageThree m n p = fromInteger (m + n + p) / 3;

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage m n p = sum [1 | x <- [m,n,p], fromInteger x > averageThree m n p]

howManyAboveAverage' :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage' m n p
	| avg < fromInteger m && avg < fromInteger n && avg < fromInteger p = 3
	| avg < fromInteger m && avg < fromInteger n ||
	  avg < fromInteger m && avg < fromInteger p ||
	  avg < fromInteger n && avg < fromInteger p
	  	= 2
	| otherwise = 1
	where avg = averageThree m n p

-- 3.22)
numberNDroots :: (Fractional a, Ord a) => a -> a -> a -> Int
numberNDroots a b c
	| b^2 > 4.0*a*c  = 2
	| b^2 == 4.0*a*c = 1
	| otherwise      = 0

infRoots :: (Integral a) => a
infRoots = 3

numberDroots :: (Fractional a, Ord a) => a -> a -> a -> Int
numberDroots a b c
	| b /= 0.0             = 1
	| b == 0.0 && c /= 0.0 = 0
	| otherwise            = infRoots

-- 3.23)
numberRoots :: (Fractional a, Ord a) => a -> a -> a -> Int
numberRoots a b c
	| a /= 0.0  = numberNDroots a b c 
	| otherwise = numberDroots  a b c

-- 3.24)
smallerRoot, largerRoot :: (Floating a, Ord a) => a -> a -> a -> a
smallerRoot a b c
	| a == 0.0  = if numberDroots a b c > 0 && numberDroots a b c /= infRoots
			      then -c/b
		          else 0.0
	| otherwise = if numberNDroots a b c > 0
		          then if sqrt(b^2 - 4*a*c)/a >= 0.0
		          	   then ((-b) - sqrt(b^2 - 4*a*c)) / (2*a)
		          	   else ((-b) + sqrt(b^2 - 4*a*c)) / (2*a)
		          else 0.0

largerRoot a b c
	| a == 0.0  = if numberDroots a b c > 0 && numberDroots a b c /= infRoots
			      then -c/b
		          else 0.0
	| otherwise = if numberNDroots a b c > 0
		          then if sqrt(b^2 - 4*a*c)/a >= 0.0
		          	   then ((-b) + sqrt(b^2 - 4*a*c)) / (2*a)
		          	   else ((-b) - sqrt(b^2 - 4*a*c)) / (2*a)
		          else 0.0

prop_quadRoots :: (Floating a, Ord a) => a -> a -> a -> Bool
prop_quadRoots a b c = smallerRoot a b c <= largerRoot a b c

prop_quadRoots2 :: (Floating a, Ord a) => a -> a -> a -> Bool
prop_quadRoots2 a b c
	| numberRoots a b c > 0 && numberRoots a b c /= infRoots
	 	= abs (a*sroot + b*sroot + c) <= error &&
	 	  abs (a*lroot + b*lroot + c) <= error
	| otherwise = True
	where sroot = smallerRoot a b c
	      lroot = largerRoot a b c
	      error = 0.01
