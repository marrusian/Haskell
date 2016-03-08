import Test.QuickCheck
import Prelude hiding (sum)

sum :: (Num a) => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

doubleAll :: (Num a) => [a] -> [a]
doubleAll []     = [] 
doubleAll (z:zs) = 2*z : doubleAll zs

prop_SumDoubleAll :: (Num a, Eq a) => [a] -> Bool
prop_SumDoubleAll xs = sum (doubleAll xs) == 2 * sum xs
