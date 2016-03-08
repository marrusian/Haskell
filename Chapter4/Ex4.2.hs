import PicturesSVG

fourPics :: Picture -> Picture
fourPics pic =
    left `beside` right
      where
        left = pic `above` invertColour pic
        right = invertColour (flipV pic) `above` flipV pic

fourPics2 :: Picture -> Picture
fourPics2 pic =
    left `beside` right
      where
        left = pic `above` invertColour pic
        right = invertColour flipped `above` flipped
        flipped = flipV pic

fourPics3 :: Picture -> Picture
fourPics3 pic =
    left `beside` right
      where
        left = pic `above` invertColour pic
        right = invertColour (flipV left)

fourPics4 :: Picture -> Picture
fourPics4 pic =
    left `beside` right
      where
        stack p = p `above` invertColour p
        left = stack pic
        right = stack (invertColour (flipV pic))

-- 4.5)
fourPics5 :: Picture -> Picture
fourPics5 pic =
    left `beside` right
      where
        stack p = p `above` invertColour p
        left = stack pic
        right = invertColour (flipV left)

-- 4.6)
fourPics' :: Picture -> Picture
fourPics' pic =
    top `above` bottom
      where
        top    = pic `beside` invertColour (flipV pic)
        bottom = invertColour pic `beside` flipV pic

fourPics'2 :: Picture -> Picture
fourPics'2 pic =
    top `above` bottom
      where
        top    = pic `beside` invertColour flipped
        bottom = invertColour pic `beside` flipped
        flipped = flipV pic

{-
-- Should work, but it doesn't! (???)
fourPics'3 :: Picture -> Picture
fourPics'3 pic =
    top `above` bottom
      where
        merge p = p `beside` invertColour (flipV pic)
        top    = merge pic
        bottom = merge (invertColour pic)
        -- (invertColour pic) `beside` invertColour (flipV (invertColour pic))
        -- ipic `beside` invertColour (fipic)
        -- ipic `beside fpic
-}

fourPics'4 :: Picture -> Picture
fourPics'4 pic =
    top `above` bottom
      where
        top    = pic `beside` invertColour (flipV pic)
        bottom = flipV top

-- 4.8)
triArea :: Double -> Double -> Double -> Double
triArea a b c
    | possible  = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise = 0
    where
      s = (a+b+c)/2
      possible
        | checkSides a b c && triIneq a b c = True
        | otherwise                         = False
        where
          checkSides a b c
            | a > 0.0 && b > 0.0 && c > 0.0 = True
            | otherwise                     = False
          triIneq a b c
            | (a < b + c) && (b < a + c) && (c < a + b) = True
            | otherwise                                 = False

-- 4.9)
maxThreeOccurs :: (Integral a) => a -> a -> a -> (a, a)
maxThreeOccurs m n p = (maxi, occurence maxi)
  where
    maxi = maximum [m,n,p]
    occurence x
      | threeEqual x = 3
      | twoEqual x   = 2
      | otherwise    = 1
      where
        threeEqual y = (y == m) && (y == n) && (y == p)
        twoEqual   y = y == m && y == n ||
                       y == m && y == p ||
                       y == n && y == p

-- 4.10)
{-
maxThreeOccurs 4 5 5
  (maxi, occurence maxi)     (definition)
    | where
    | maxi = maximum [4,5,5] = 5
    | occurence 5 =
      ?? threeEqual 5
        | where
        | threeEqual 5 = (5 == 4) && (5 == 5) && (5 == 5) = False && True && True = False
      ?? twoEqual 5
        | where
        | twoEqual 5 = 5 == 4 && 5 == 5 || 5 == 4 && 5 == 5 || 5 == 5 && 5 == 5
                    = False && True || False && True || True && True
                    = False || False || True
                    = True
      = 2
(5, 2)
-}