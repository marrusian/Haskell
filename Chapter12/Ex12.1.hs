module Ex121 where
    import Data.Bool

    type Picture = [[Char]]

    flipH :: Picture -> Picture
    flipH = reverse

    flipV :: Picture -> Picture
    flipV = map reverse

    printPicture :: Picture -> IO ()
    printPicture = putStr . concat . map (++ "\n")

    above :: Picture -> Picture -> Picture
    above = (++)

    beside :: Picture -> Picture -> Picture
    beside = zipWith (++)

    invertChar :: Char -> Char
    invertChar ch
        | ch == '.' = '#'
        | otherwise = '.'

    invertColour :: Picture -> Picture
    invertColour = map (map invertChar)

    combineChar :: Char -> Char -> Char
    combineChar topCh bottomCh
        | topCh == '.' && bottomCh == '.' = '.'
        | otherwise                       = '#'

    superimpose :: Picture -> Picture -> Picture
    superimpose = zipWith (zipWith combineChar)

    -- 12.1)
    bwLine :: Int -> [Char]
    bwLine n = [if i `mod` 2 == 0 then '#' else '.' | i <- [1..n]]

    wbLine :: Int -> [Char]
    wbLine n = [if i `mod` 2 == 0 then '.' else '#' | i <- [1..n]]

    chessBoard :: Int -> Picture
    chessBoard n = [if i `mod` 2 == 0 then wbLine n else bwLine n | i <- [1..n]]

    -- 12.2)
    type Picture' = [[Bool]]

    invertColour' :: Picture' -> Picture'
    invertColour' = map (map not)

    superimpose' :: Picture' -> Picture' -> Picture'
    superimpose' = zipWith (zipWith (&&))

    printPicture' :: Picture' -> IO ()
    printPicture' = putStr . concat . map ((++ "\n") . map (bool '.' '#'))

    -- 12.3)
    makePicture :: Int -> Int -> [(Int, Int)] -> Picture
    makePicture n m []  = replicate m (replicate n '.')
    makePicture n m pts = [[if (i,j) `elem` pts then '#' else '.' | j <- [0..n-1]] | i <- [0..m-1]]

    -- 12.4)
    unmakePicture :: Picture -> (Int, Int, [(Int, Int)])
    unmakePicture pic = (m, n, blackPoints)
        where n = length pic
              m = length (head pic)
              dummy = (-1,-1)
              mapPoints = [if ch == '#' then (i,j) else dummy | i <- [0..n-1], j <- [0..m-1], let ch = pic !! i !! j]
              blackPoints = filter (dummy /=) mapPoints
