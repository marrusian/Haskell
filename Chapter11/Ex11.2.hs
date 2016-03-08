module Ex112 where
    mapFuns :: [a -> b] -> a -> [b]
    mapFuns (f:fs) x = f x : mapFuns fs x
    mapFuns _ _      = []

    mapFuns' :: [a -> b] -> a -> [b]
    mapFuns' fs x = map ($ x) fs

    mapFuns'' :: [a -> b] -> a -> [b]
    mapFuns'' fs x = map (\f -> f x) fs

    addNum :: Integer -> (Integer -> Integer)
    addNum n = (\m -> n+m)

    comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
    comp2 f g = (\x y -> g (f x) (f y))

    -- 11.7)
    isNotSpace :: Char -> Bool
    isNotSpace = (\ch -> not $ ch `elem` whitespace)
        where whitespace = " \t\n"

    -- 11.8)
    total :: (Integer -> Integer) -> (Integer -> Integer)
    total f = (\n -> foldl ((+) . f) 0 [0..n])

    -- 11.9, 11.10)
    revFunc :: (a -> b -> c) -> (b -> a -> c)
    revFunc f = (\x y -> f y x)
