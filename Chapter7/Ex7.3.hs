import Prelude hiding (sum, product, and, or)
import qualified Prelude

import Test.QuickCheck

sum :: (Num a) => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

prop_sum :: (Eq a, Num a) => [a] -> Bool
prop_sum xs = sum xs == Prelude.sum xs

-- 7.5)
product :: (Num a) => [a] -> a
product []     = 1
product (x:xs) = x * product xs

prop_product :: (Eq a, Num a) => [a] -> Bool
prop_product xs = product xs == Prelude.product xs

-- 7.6)
and, or :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or []      = False
or (x:xs)  = x || or xs

prop_and :: [Bool] -> Bool
prop_and xs = and xs == Prelude.and xs

prop_or :: [Bool] -> Bool
prop_or xs = or xs == Prelude.or xs