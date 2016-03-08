isAlpha :: Char -> Bool
isAlpha ch = ('A' <= ch) && (ch <= 'z')

isAlpha' :: Char -> Bool
isAlpha' = (`elem` ['A'..'z'])

isUpper :: Char -> Bool
isUpper ch = ('A' <= ch) && (ch <= 'Z')

isUpper' :: Char -> Bool
isUpper' = (`elem` ['A'..'Z'])

--3.16)
isLower :: Char -> Bool
isLower ch = ('a' <= ch) && (ch <= 'z')

isLower' :: Char -> Bool
isLower' = (`elem` ['a'..'z'])

toUpper :: Char -> Char
toUpper ch
    | isLower ch = toEnum (fromEnum ch + offset)
    | otherwise  = ch
    where offset = fromEnum 'A' - fromEnum 'a'

toUpper' :: Char -> Char
toUpper' ch
    | isLower' ch = toEnum (fromEnum ch + offset)
    | otherwise   = ch
    where offset  = fromEnum 'A' - fromEnum 'a'

-- 3.17)
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

isDigit' :: Char -> Bool
isDigit' = (`elem` ['0'..'9'])

charToNum :: Char -> Int
charToNum ch
    | isDigit ch = toEnum (fromEnum ch - fromEnum '0')
    | otherwise = 0

charToNum' :: Char -> Int
charToNum' ch
    | isDigit' ch = toEnum (fromEnum ch - fromEnum '0')
    | otherwise = 0