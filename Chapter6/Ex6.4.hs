import Pictures hiding (printPicture, prop_AboveFlipV, prop_AboveFlipH, prop_AboveBeside, rectangular)
import Test.QuickCheck

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 = flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2)

{-
prop_AboveBeside :: Picture -> Picture -> Property
prop_AboveBeside w e =
 (rectangular w && rectangular e && height w == height e)
                        ==> (w `beside` e) `above` (w `beside` e)
                             ==
                            (w `above` w) `beside` (e `above` e)
-}

-- 6.4)
superimposeChar :: Char -> Char -> Char
superimposeChar ch1 ch2
    | ch1 == '.' && ch2 == '.' = '.'
    | otherwise                = '#'

-- 6.5)
superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine line1 line2 = [superimposeChar ch1 ch2 | (ch1, ch2) <- zip line1 line2]

-- 6.6)
superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine line1 line2 | (line1, line2) <- zip pic1 pic2]

-- 6.7)
printPicture :: Picture -> IO()
printPicture pic = putStr (concat [line ++ "\n" | line <- pic])

-- 6.8)
rotate90 :: Picture -> Picture
rotate90 pic = [[line !! i | line <- flipH pic] | i <- [0..length (head pic) - 1]]

-- 6.9)
rotate90AC :: Picture -> Picture
rotate90AC pic = rotate (rotate90 pic)

rotate90AC' :: Picture -> Picture
rotate90AC' pic = [[line !! i | line <- flipV pic] | i <- [0..length (head pic) - 1]]

-- 6.10)
scale :: Picture -> Int -> Picture
scale pic n
    | n > 0 = [[ch | ch <- line, j <- [1..n]] | line <- pic, i <- [1..n]]
    | otherwise = [[]]

-- 6.11)
prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)

-- 6.12)
prop_BesideFlipV :: Picture -> Picture -> Bool
prop_BesideFlipV pic1 pic2 = flipV (pic1 `beside` pic2) == (flipV pic2) `beside` (flipV pic1)

prop_BesideFlipH :: Picture -> Picture -> Bool
prop_BesideFlipH pic1 pic2 = flipH (pic1 `beside` pic2) == (flipH pic1) `beside` (flipH pic2)

-- 6.13)
prop_AboveBeside :: Picture -> Bool
prop_AboveBeside pic = line `above` line == column `beside` column
    where 
        line   = pic `beside` pic
        column = pic `above`  pic

-- 6.14)
rectangular :: Picture -> Bool
rectangular [] = False
rectangular (first:rest) = and [length first == length line | line <- rest]
