-- Example 1
power2 :: (Integral a) => a -> Double
power2 0 = 1
power2 n
    | n > 0 = power2 (n-1) * 2
    | n < 0 = 1/power2 (-n)

-- Example 2
sumFun :: (Integral a, Num b) => (a -> b) -> a -> b
sumFun f 0 = f 0
sumFun f n
    | n > 0 = sumFun f (n-1) + f n
    | n < 0 = error "sumFun: Function only defined on natural numbers"

-- Example3
regions :: (Integral a) => a -> a
regions 0 = 1
regions n
    | n > 0 = regions (n-1) + n
    | n < 0 = error "regions: Function only defined on natural numbers"

-- 4.19)
mul :: (Integral a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul n m
    | signum n == signum m =  positive_mul (abs n) (abs m)
    | otherwise            = -positive_mul (abs n) (abs m)
    where
        positive_mul n m
            | n <= m = m + mul m (n-1)
            | m < n  = n + mul n (m-1)

-- 4.20)
isqrt :: (Integral a) => a -> a
isqrt 0 = 0
isqrt n
    | n > 0 = find_isqr n 1 
    | n < 0 = error "isqrt: Function only defined on natural numbers"
    where
        find_isqr n m
            | m*m <= n && (m+1)*(m+1) > n = m 
            | otherwise                   = find_isqr n (m+1)

-- 4.21)
maxFunc :: (Integral a) => (a -> a) -> a -> a
maxFunc f 0 = f 0
maxFunc f n
    | n > 0 = max (f n) (maxFunc f (n-1))
    | n < 0 = error "maxFunc: Function only defined on natural numbers"

-- 4.22)
isZero :: (Integral a) => (a -> a) -> a -> Bool
isZero f 0 = if (f 0 == 0) then True else False
isZero f n
    | n > 0 = if (f n == 0) then True else isZero f (n-1)
    | n < 0 = error "isZero: Function only defined on natural numbers"

-- 4.23) [WARNING: EXTREMELY INEFFICIENT]
regions' :: (Integral a) => a -> a
regions' 0 = 1
regions' 1 = 2
regions' n
    | n >= 2 = n + sumFun regions' (n-1) - sumFun regions' (n-2)
    | n < 0  = error "regions: Function only defined on natural numbers"

{-
sumFun f n-1 = f n-1 + f n-2 + ... + f 1 + f 0
sumFun f n-2 =         f n-2 + ... + f 1 + f 0
=> sumFun f (n-1) + sumFun f (n-2) = f n-1

regions' n = n + regions' n-1 = n + n-1 + regions' n-2 =
           = ... = n + n-1 + ... + 2 + 1 + 1 = n*(n+1)/2 + 1
-}

-- The most efficient implementation [direct calculation]
regions'' :: (Integral a) => a -> a
regions'' n = n*(n+1) `div` 2 + 1
