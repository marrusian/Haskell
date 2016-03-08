exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

exOr1 :: Bool -> Bool -> Bool
exOr1 True x = not x
exOr1 False x = x

myNot :: Bool -> Bool
myNot True  = False
myNot False = True

-- 3.1)
exOr2 :: Bool -> Bool -> Bool
exOr2 x y = (x == True && y == False) || (x == False && y == True)

-- 3.3)
exOr3 :: Bool -> Bool -> Bool
exOr3 True True   = False
exOr3 True False  = True
exOr3 False True  = True
exOr3 False False = False

-- 3.4)
myAnd :: Bool -> Bool -> Bool
myAnd True x  = x
myAnd False x = False

myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr False x = x

-- 3.5)
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd1 :: Bool -> Bool -> Bool
nAnd1 True x  = not x 
nAnd1 False x = True

-- 3.6)
{-
nAnd True True
not (True && True)   (definition nAnd)
not (True)           (boolean evaluation)
False                (boolean evaluation)

nAnd True False
not (True && False)  (definition nAnd)
not (False)          (boolean evaluation)
True                 (boolean evaluation)

nAnd1 True True
not True             (definition nAnd1)
False                (boolean evaluation)

nAnd1 True False
not False            (definition nAnd1)
True                 (boolean evaluation)
-}

-- 3.7)
prop_myNot :: Bool -> Bool
prop_myNot x = not x == myNot x

prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y = exOr x y == exOr1 x y

prop_exOr2 :: Bool -> Bool -> Bool
prop_exOr2 x y = exOr x y == (x /= y)

prop_myAnd :: Bool -> Bool -> Bool
prop_myAnd x y = myAnd x y == (x && y)

prop_myOr :: Bool -> Bool -> Bool
prop_myOr x y = myOr x y == x || y

prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y = nAnd x y == nAnd1 x y