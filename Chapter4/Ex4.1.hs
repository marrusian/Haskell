-- 4.1)
maxThree :: (Ord a) => a -> a -> a -> a
maxThree x y z
    | x >= y && x >= z = x
    | y >= z           = y
    | otherwise        = z 

maxFour :: (Ord a) => a -> a -> a -> a -> a
maxFour m n p q
    | m >= n && m >= p && m >= q = m
    | n >= p && n >= q           = n
    | p >= q                     = p
    | otherwise                  = q

maxFour' :: (Ord a) => a -> a -> a -> a -> a
maxFour' m n p q = max (max m n) (max p q)

maxFour'' :: (Ord a) => a -> a -> a -> a -> a
maxFour'' m n p q = max (maxThree m n p) q

-- 4.2)
weakAscendingOrder :: (Ord a) => a -> a -> a -> Bool
weakAscendingOrder m n p
	| (m <= n) && (n <= p) = True
	| otherwise            = False

strongAscendingOrder :: (Ord a) => a -> a -> a -> Bool
strongAscendingOrder m n p
	| (m < n) && (n < p) = True
	| otherwise          = False

between :: (Ord a) => a -> a -> a -> Bool
between m n p
    | weakAscendingOrder m n p   = True
    | otherwise                  = False

middleNumber :: (Integral a) => a -> a -> a -> a
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise     = z

-- 4.3)
twoEqual :: (Eq a) => a -> a -> a -> Bool
twoEqual x y z = (x == y) || (x == z) || (y == z)

threeEqual :: (Eq a) => a -> a -> a -> Bool
threeEqual x y z = (x == y) && (y == z)

fourEqual :: (Eq a) => a -> a -> a -> a -> Bool
fourEqual x y z w = (x == y) && (y == z) && (z == w)

howManyEqual :: (Eq a) => a -> a -> a -> Integer
howManyEqual x y z
	| x == y && y == z           = 3
	| x == y || x == z || y == z = 2
	| otherwise                  = 0

howManyEqual' :: (Eq a) => a -> a -> a -> Integer
howManyEqual' x y z
	| threeEqual x y z  = 3
	| twoEqual x y z    = 2
	| otherwise         = 0

-- 4.4)
howManyOfFourEqual :: (Eq a) => a -> a -> a -> a -> Integer
howManyOfFourEqual x y z w
	| x == y && y == z && z == w = 4
	| x == y && y == z ||
	  x == z && z == w ||
	  y == z && z == w           = 3
	| x == y || x == z ||
      x == w || y == z ||
      y == w || z == w           = 2
    | otherwise                  = 0

howManyOfFourEqual' :: (Eq a) => a -> a -> a -> a -> Integer
howManyOfFourEqual' x y z w
    | fourEqual x y z w = 4
    | otherwise         = maximum [howManyEqual' x y z, howManyEqual' x y w, howManyEqual' x z w, howManyEqual' y z w]