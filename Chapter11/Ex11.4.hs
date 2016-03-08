module Ex114 where
    import Prelude hiding (curry, uncurry)
    import qualified Prelude
    import Test.QuickCheck

    curryPA :: ((a,b) -> c) -> (a -> b -> c)
    curryPA g = (\x y -> g (x,y))

    curry :: ((a,b) -> c) -> (a -> b -> c)
    curry g x y = g (x,y)

    uncurryPA :: (a -> b -> c) -> ((a,b) -> c)
    uncurryPA g = (\(x,y) -> g x y)

    uncurry :: (a -> b -> c) -> ((a,b) -> c)
    uncurry g (x,y) = g x y

    prop_Curry :: (Eq c) => ((a,b) -> c) -> a -> b -> Bool
    prop_Curry g x y = curry g x y == Prelude.curry g x y

    prop_Uncurry :: (Eq c) => (a -> b -> c) -> (a,b) -> Bool
    prop_Uncurry g (x,y) = uncurry g (x,y) == Prelude.uncurry g (x,y)

    prop_zip :: (Eq a, Eq b) => [(a,b)] -> Bool
    prop_zip xs = uncurry zip (unzip xs) == xs

    -- 11.14)
    -- ($) :: (a -> b) -> a -> b
    -- uncurry ($) :: (a -> b, a) -> b

    -- (:) :: a -> [a] -> [a]
    -- uncurry (:) :: (a, [a]) -> [a]

    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- uncurry (.) :: (b -> c, a -> b) -> a -> c

    -- 11.15)
    -- uncurry :: (a -> b -> c) -> (a,b) -> c
    -- uncurry uncurry :: (a -> b -> c, (a,b)) -> c
    -- Example: uncurry uncurry (zip, ([1,2], [3,4])) = [(1,3), (2,4)]

    -- curry :: ((a,b) -> c) -> a -> b -> c
    -- curry uncurry :: Type error, because there is nothing to curry.
    
    -- 11.16)
    prop_unzip :: (Eq a, Eq b) => [a] -> [b] -> Bool
    prop_unzip xs ys = (unzip $ uncurry zip (xs, ys)) == (xs, ys) || length xs /= length ys

    -- 11.17)
    curry3PA :: ((a,b,c) -> d) -> (a -> b -> c -> d)
    curry3PA g = (\x y z -> g (x,y,z))

    curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
    curry3 g x y z = g (x,y,z)

    uncurry3PA :: (a -> b -> c -> d) -> ((a,b,c) -> d)
    uncurry3PA g = (\(x,y,z) -> g x y z)

    uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
    uncurry3 g (x,y,z) = g x y z

    -- 11.18) ???
    curryList :: ([a] -> d) -> (a -> [a] -> d)
    curryList fs = (\x xs -> fs (x:xs))

    uncurryList :: (a -> [a] -> d) -> ([a] -> d)
    uncurryList fs = (\(x:xs) -> fs x xs)

