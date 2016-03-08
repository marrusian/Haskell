import Data.Char

-- 3.18)
onThreeLines :: String -> String -> String -> String
onThreeLines str1 str2 str3 = str1 ++ "\n" ++ str2 ++ "\n" ++ str3 ++ "\n"

-- 3.19)
romanDigit :: Char -> String
romanDigit rdig
    | rdig == '1' = "I"
    | rdig == '2' = "II"
    | rdig == '3' = "III"
    | rdig == '4' = "IV"
    | rdig == '5' = "V"
    | rdig == '6' = "VI"
    | rdig == '7' = "VII"
    | rdig == '8' = "VIII"
    | rdig == '9' = "IX"
    | otherwise = error ("romanDigit: not a digit " ++ show rdig)
