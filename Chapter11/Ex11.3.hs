module Ex113 where
    import Test.QuickCheck

    multiply :: Int -> Int -> Int
    multiply x y = x*y

    doubleAll :: [Int] -> [Int]
    doubleAll = map (multiply 2)

    whitespace = " \t\n"

    dropSpace :: String -> String
    dropSpace = dropWhile (`elem` whitespace)

    dropWord :: String -> String
    dropWord = dropWhile $ not . (`elem` whitespace)

    getWord :: String -> String
    getWord = takeWhile $ not . (`elem` whitespace)

    -- 11.12)
    prop_Section :: (Ord a, Num a) => [a] -> Bool
    prop_Section xs = (map (+1) . filter (> -1)) xs == (filter (>0) . map (+1)) xs