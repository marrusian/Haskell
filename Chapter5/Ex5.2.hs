data Move = Rock | Paper | Scissors
            deriving (Eq, Show)

data People = Person Name Age
              deriving (Eq, Show)

type Name = String
type Age  = Int

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

data Shape = Circle Double | 
             Rectangle Double Double
             deriving (Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _)      = True
isRound (Rectangle _ _) = False

area :: Shape -> Double
area (Circle r)      = pi*r*r
area (Rectangle h w) = h*w

-- 5.5)
perimeter :: Shape -> Double
perimeter (Circle r)      = 2*pi*r
perimeter (Rectangle h w) = 2*h + 2*w

-- 5.6)
data Item = ShopItem String Double
            deriving (Eq, Show)

-- 5.7)
data Shape' = Circle' Radius |
              Rectangle' Height Width |
              Triangle' Side Side Side
              deriving (Eq, Ord, Show)

type Radius = Double
type Height = Double
type Width  = Double
type Side   = Double

triangleShape' :: Double -> Double -> Double -> Shape'
triangleShape' x y z
    | isTriangle x y z = Triangle' x y z
    | otherwise        = error "The sides do not form a triangle"

isTriangle :: (Floating a, Ord a) => a -> a -> a -> Bool
isTriangle x y z
    | x < y+z && y < x+z && z < x+y = True
    | otherwise                     = False

isRound' :: Shape' -> Bool
isRound' (Circle' _)       = True
isRound' (Rectangle' _ _)  = False
isRound' (Triangle' _ _ _) = False

pitagora :: (Floating a) => a -> a -> a
pitagora c1 c2 = sqrt(c1*c2 + c2*c2)

area' :: Shape' -> Double
area' (Circle' r) = pi*r*r
area' (Rectangle' h w) = h*w
area' (Triangle' s1 s2 s3) = heron s1 s2 s3
    where
        heron x y z = sqrt(s*(s-x)*(s-y)*(s-z))
            where
                s = (x+y+z)/2

perimeter' :: Shape' -> Double
perimeter' (Circle' r)          = 2*pi*r
perimeter' (Rectangle' h w)     = 2*h + 2*w
perimeter' (Triangle' s1 s2 s3) = s1+s2+s3

-- 5.8)
isRegular :: Shape' -> Bool
isRegular (Circle' r)          = True
isRegular (Rectangle' h w)     = if (h == w) then True else False
isRegular (Triangle' s1 s2 s3) = if (s1 == s2 && s2 == s3) then True else False

-- 5.11)
data NewShape = CircleN Radius Centre |
                RectangleN Height Width Centre |
                TriangleN Side Side Side Centre
                deriving (Eq, Ord, Show)

type Centre = (Double, Double)

-- 5.12)
move :: Double -> Double -> NewShape -> NewShape
move x y (CircleN r (a,b))          = CircleN r (a+x,b+y)
move x y (RectangleN h w (a,b))     = RectangleN h w (a+x,b+y)
move x y (TriangleN s1 s2 s3 (a,b)) = TriangleN s1 s2 s3 (a+x,b+y)

{-
distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = sqrt((x2-x1)**2 + (y2-y1)**2)

centreCoord :: NewShape -> Centre
centreCoord (CircleN _ (a,b))        = (a,b)
centreCoord (RectangleN _ _ (a,b))   = (a,b)
centreCoord (TriangleN _ _ _ (a,b)) = (a,b)

-- 5.13)
isOverlaping :: NewShape -> NewShape -> Bool
isOverlaping (CircleN r1 (a1,b1)) (CircleN r2 (a2, b2)) 
    | distance (a1,b1) (a2,b2) > r1 = False
    | otherwise                     = True
-}