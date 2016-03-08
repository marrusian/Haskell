fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n
    | n > 1     = fib (n-2) + fib (n-1)
    | otherwise = error "fib: Function only defined on natural numbers"

remainder, divide :: (Integral a) => a -> a -> a
remainder m n
    | m < n     = m
    | otherwise = remainder (m-n) n

divide m n
    | m < n     = 0
    | otherwise = 1 + divide (m-n) n

-- 4.31)
gcd' :: (Integral a) => a -> a -> a
gcd' m n
    | rem m n /= 0 = gcd' n (rem m n)
    | otherwise    = rem n (m+n)

-- 4.32)
twoExp :: (Integral a) => a -> a
twoExp 0 = 1
twoExp n
    | n < 0          = error "twoExp: negative exponent"
    | n `mod` 2 == 0 = (twoExp (n `div` 2))^2
    | n `mod` 2 == 1 = 2*(twoExp ((n-1) `div` 2))^2
