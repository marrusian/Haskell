import Pictures hiding (height, width)

type Position = (Int, Int)

type Image = (Picture, Position)

-- 6.29)
makeImage :: Picture -> Position -> Image
makeImage pic pos = (pic, pos)

-- 6.30)
changePosition :: Image -> Position -> Image
changePosition (pic, _) new_pos = (pic, new_pos)

-- 6.31)
moveImage :: Image -> Int -> Int -> Image
moveImage (pic, (x, y)) xMove yMove = (pic, (x+xMove, y+yMove))

-- 6.32)
width :: Picture -> Int
width = length.head

height :: Picture -> Int
height = length

printImage :: Image -> IO()
printImage (pic, (x,y)) = printPicture ([replicate x separator | i <- [1..height pic + y]] `beside` (pic `above` [replicate (width pic) separator | i <- [1..y]]))
    where
        separator = ' '

-- 6.33)
rotate90 :: Picture -> Picture
rotate90 pic = [[line !! i | line <- flipH pic] | i <- [0..length (head pic) - 1]]

flipH_NV :: Image -> Image
flipH_NV (pic, pos) = (flipH pic, pos)

flipV_NV :: Image -> Image
flipV_NV (pic, pos) = (flipV pic, pos)

rotate_NV :: Image -> Image
rotate_NV (pic, pos) = (rotate pic, pos)

rotate90_NV :: Image -> Image
rotate90_NV (pic, pos) = (rotate90 pic, pos)

-- 6.34)
flipH_GV :: Image -> Image
flipH_GV (pic, pos) = moveImage (flipH pic, pos) 0 (height pic)

flipV_GV :: Image -> Image
flipV_GV (pic, pos) = moveImage (flipV pic, pos) (width pic) 0

rotate_GV :: Image -> Image
rotate_GV (pic, pos) = moveImage (rotate pic, pos) (width pic) (-length pic)

rotate90_GV :: Image -> Image
rotate90_GV (pic, pos) = moveImage (rotate90 pic, pos) (width pic) (-width pic)

-- 6.35)
padLeft :: Picture -> Int -> Picture
padLeft pic padding = [replicate padding '.' | i <- [1..height pic]] `beside` pic

padRight :: Picture -> Int -> Picture
padRight pic padding = pic `beside` [replicate padding '.' | i <- [1..height pic]]

padTop :: Picture -> Int -> Picture
padTop pic padding = [replicate (width pic) '.' | i <- [1..padding]] `above` pic

padBottom :: Picture -> Int -> Picture
padBottom pic padding = pic `above` [replicate (width pic) '.' | i <- [1..padding]]

padOut :: Image -> Int -> Int -> Int -> Int -> Image
padOut (pic, (x,y)) left right bottom top
    | left < 0 || right < 0 || bottom < 0 || top < 0 = error "padOut: Padding cannot be negative"
    | bottom /= 0                                    = padOut (pic, (x, y+bottom)) left right 0 top
    | left   /= 0                                    = padOut (pic, (x+left, y)) 0 right bottom top
    | otherwise                                      = (padRight (padTop pic top) right, (x,y))

-- 6.36)
distance :: (Floating a) => Position -> Position -> a
distance (x1,y1) (x2,y2) = sqrt(((toInteger x2)-(toInteger x1))^2 + ((toInteger y2)-(toInteger y1)^2))

isOverlapping :: Image -> Image -> Bool
isOverlapping (pic1, (x1,y1)) (pic2, (x2,y2)) = distance (x1,y1) (x2,y2) <= distance (x1,y1) (x1+width pic1, y1+height pic1)

{-
superImpose :: Image -> Image -> Image
superImpose img1 img2
    | 
-}