module Ex111 where
    import Test.QuickCheck

    type Picture = [[Char]]

    flipV :: Picture -> Picture
    flipV = map reverse

    flipH :: Picture -> Picture
    flipH = reverse

    rotate :: Picture -> Picture
    rotate pic = flipV (flipH pic)

    rotateC :: Picture -> Picture
    rotateC = flipV . flipH

    infixl 9 >.>
    (>.>) :: (a -> b) -> (b -> c) -> a -> c
    (>.>) g f = f . g

    prop_FC :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
    prop_FC f g x = (g >.> f) x == (f . g) x && (f . g) x == f (g x)

    rotateFC :: Picture -> Picture
    rotateFC = flipH >.> flipV

    -- 11.1)
    {-
        printBill :: TillType -> IO ()
        printBill = makeBill >.> formatBill >.> putStrLn
    -}

    -- 11.2)
    {-
        (id . f) - compose f() with the function id()
        (f . id) - compose id() with the function f()
        id f     - apply id() to f()

        If f :: Int -> Bool, then:
            (id . f) - id :: Bool -> Bool
            (f . id) - id :: Int -> Int (but there is a type error if a /= Int)
            id f     - id :: (Int -> Bool) -> (Int -> Bool)
    -}

    -- 11.3)
    composeList :: [a -> a] -> a -> a
    composeList = foldr (>.>) id 

    -- 11.4)
    -- ($) :: (a -> b) -> a -> b

    -- 11.5)
    -- zipWith ($) [sum, product] [[1,2], [3,4]] == [sum $ [1,2], product $ [3,4]]
    --                                           == [3, 12]

    -- 11.6)
    {-
        (id $ f) - apply id() to f()
        (f $ id) - apply f() to id
        id ($)   - apply id() to the application operator ($)

        If f :: Int -> Bool, then:
            (id $ f) - id :: (Int -> Bool) -> (Int -> Bool)
            (f $ id) - id :: Int -> Int
            id ($)   - id :: ((a -> b) -> a -> b) -> ((a -> b) -> a -> b)
    -}
