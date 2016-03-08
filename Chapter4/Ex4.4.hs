fact :: (Integral a) => a -> a
fact 0 = 1
fact n
    | n > 0 = fact (n-1) * n
    | otherwise = error "Factorial can't be negative"

-- 4.17)
rangeProduct :: (Integral a) => a -> a-> a
rangeProduct m n
    | n < 0 || m < 0 = error "rangeProduct() only defined on natural numbers"
    | n == m         = n
    | n > m          = rangeProduct m (n-1) * n
    | otherwise      = error ("arg1 (" ++ show (toInteger m)   ++ 
                              ") can't be smaller than arg2 (" ++
                              show (toInteger n) ++ ")")

-- 4.18)
fact' :: (Integral a) => a -> a
fact' 0 = 1
fact' n = rangeProduct 1 n