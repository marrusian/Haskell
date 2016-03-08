import PicturesSVG

blackSquares :: Integer -> Picture
blackSquares n
    | n <= 1    = black
    | otherwise = black `beside` blackSquares (n-1)

blackWhite :: Integer -> Picture
blackWhite n
    | n <= 1    = black
    | otherwise = black `beside` whiteBlack (n-1)

blackChess :: Integer -> Integer -> Picture
blackChess n m
    | n <= 1    = blackWhite m
    | otherwise = blackWhite m `above` whiteChess (n-1) m

-- 4.25)
whiteBlack :: Integer -> Picture
whiteBlack n
    | n <= 1    = white
    | otherwise = white `beside` blackWhite (n-1)

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
    | n <= 1    = whiteBlack m
    | otherwise = whiteBlack m `above` blackChess (n-1) m

-- 4.26)
column :: Picture -> Integer -> Picture
column pic n
    | n <= 1    = pic
    | otherwise = pic `above` column pic (n-1)

-- 4.27)
whiteSquares :: Integer -> Picture
whiteSquares n
    | n <= 1    = white
    | otherwise = white `beside` whiteSquares (n-1)

blackNLine :: Integer -> Integer -> Picture
blackNLine n m
    | n <= 1         = black
    | m < 1 || m > n = error ("blackNLine: arg2 isn't in [" ++ show 1 ++ ", " ++ show n ++ "]")
    | m == 1         = black `beside` whiteSquares (n-m)
    | m == n         = whiteSquares (m-1) `beside` black
    | otherwise      = (whiteSquares (m-1) `beside` black) `beside` whiteSquares (n-m)

blackNPDM :: Integer -> Integer -> Picture
blackNPDM n m
    | n <= 1    = black
    | m < 1 || m > n = error ("blackNPDM: arg2 isn't in [" ++ show 1 ++ ", " ++ show n ++ "]")
    | m == n    = blackNLine n m
    | m <= n    = blackNLine n m `above` blackNPDM n (m+1)

blackPrimaryDiagonalMatrix :: Integer -> Picture
blackPrimaryDiagonalMatrix n = blackNPDM n 1

-- 4.28)
blackSecondaryDiagonalMatrix :: Integer -> Picture
blackSecondaryDiagonalMatrix n = flipH (blackNPDM n 1)
