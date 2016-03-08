module Ex115 where
    import Prelude hiding (flip)
    import qualified Prelude
    import Test.QuickCheck

    twice :: (a -> a) -> (a -> a)
    twice f = f.f

    iter :: (Integral i) => i -> (a -> a) -> (a -> a)
    iter n f
        | n > 0     = f . iter (n-1) f
        | otherwise = id

    double :: (Num a) => a -> a
    double = (*2)

    twoExp :: (Integral i, Num a) => i -> a
    twoExp n = iter n double $ 1

    -- 11.19)
    {-
        iter 3 double $ 1
        -> 3>0? double . iter 2 double
        -> 2>0? double . double . iter 1 double
        -> 1>0? double . double . double . iter 0 double
        -> 0>0? double . double . double . id
        -> double (double (double (id 1)))
        -> double (double (double 1))
        -> double (double (1*2))
        -> double ((1*2)*2)
        -> ((1*2)*2)*2
        -> (2*2)*2
        -> 4*2
        -> 8

        (comp2 succ (*)) 3 4
        -> g (f x) (f y)
        -> (*) (succ 3) (succ 4)
        -> (*) 4 5
        -> 20

        comp2 (\x -> x*x) (+) 3 4
        -> g (f x) (f y)
        -> (+) (\3 -> 3*3) (\4 -> 4*4)
        -> (+) 3*3 4*4
        -> (+) 9 16
        -> 25
    -}

    -- 11.20)
    -- (\n -> iter n succ) :: (Integral i, Enum a) => i -> a -> a

    -- 11.21)
    iter' :: Int -> (a -> a) -> (a -> a)
    iter' n f = foldr (.) id (replicate n f)

------------------------------------------------------------------------
    addNum :: (Integral a) => a -> a -> a
    addNum n = addN
        where 
            addN m = n + m

    addNum' :: (Integral a) => a -> a -> a
    addNum' n = let addN m = n+m in addN

    flip :: (a -> b -> c) -> (b -> a -> c)
    flip f = (\x y -> f y x)

    type Name = String
    type Age  = Int

    data People = Person Name Age
                  deriving (Eq, Show)

    zipPeople :: [String] -> [Int] -> [People]
    zipPeople = zipWith Person

    doubleAll :: (Integral a) => [a] -> [a]
    doubleAll = map (*2)

    getEven :: (Integral a) => [a] -> [a]
    getEven = filter ((==0) . (`mod` 2))

    getUntil :: (a -> Bool) -> [a] -> [a]
    getUntil p = takeWhile (not . p)

    getWord :: String -> String
    getWord = getUntil (`elem` whitespace)
        where
            whitespace = " \t\n"

    -- 11.22)
    mapFuns :: [a->b] -> a -> [b]
    mapFuns fs x = map ($x) fs

    -- 11.23)
    derivative :: (Floating a, Eq a) => (a -> a) -> (a -> a)
    derivative f x = (f (x+h) - f x) / h
        where
            h = sqrt mEps

    findMachineEpsilon:: (Fractional a, Eq a) => a -> a
    findMachineEpsilon eps
        | 1.0 + 0.5 * eps /= 1.0 = findMachineEpsilon (0.5 * eps)
        | otherwise              = eps

    mEps :: (Fractional a, Eq a) => a
    mEps = findMachineEpsilon 1.0

    -- 11.24)
    norm :: (Fractional a, Integral i) => a -> a -> i -> a
    norm a b n = (b - a) / fromIntegral n

    samplingPoints :: (Fractional a, Integral i) => a -> a -> i -> [a]
    samplingPoints a b n = [a + mesh * (fromIntegral i) | i <- [1..n]]
        where
            mesh = norm a b n

    integrate :: (Fractional a) => (a -> a) -> a -> a -> a
    integrate f a b = sum $ map ((*mesh) . f) ci
        where
            ci   = samplingPoints a b n
            mesh = norm a b n
            n    = 10^6
  
