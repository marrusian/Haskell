import Pictures hiding (width, height)
import Test.QuickCheck

-- 6.17)
width :: Picture -> Int
width = length.head

height :: Picture -> Int
height = length


above' :: Picture -> Picture -> Picture
above' pic1 pic2
    | width1 == width2 = above pic1 pic2
    | width1 < width2  = above (pic1 `beside` [replicate (width2-width1) '.' | i <- [1..length pic1]]) pic2
    | otherwise        = above pic1 (pic2 `beside` [replicate (width1-width2) '.' | i <- [1..length pic2]])
    where
        width1 = width pic1
        width2 = width pic2

beside' :: Picture -> Picture -> Picture
beside' pic1 pic2
    | height1 == height2 = beside pic1 pic2
    | height1 < height2  = (pic1 `above` [replicate (length pic1) '.' | i <- [1..height2-height1]]) `beside` pic2
    | otherwise          = pic1 `beside` (pic2 `above` [replicate (length pic2) '.' | i <- [1..height1-height2]])
    where
        height1 = height pic1
        height2 = height pic2

-- 6.18)
rectangularize :: Picture -> Picture
rectangularize pic = [line ++ replicate (maxHeight - (length line)) '.' | line <- pic]
    where
        maxHeight = maximum [length line | line <- pic]
