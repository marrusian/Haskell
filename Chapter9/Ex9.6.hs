import Test.QuickCheck
import Prelude hiding (length, (++), reverse, elem, zip, unzip, take, drop)

-- 1)
length :: [a] -> Int
length []     = 0
length (z:zs) = 1 + length zs

(++) :: [a] -> [a] -> [a]
(++)   []   zs = zs
(++) (w:ws) zs = w : (ws ++ zs)

prop_lengthPlusPlus :: [a] -> [a] -> Bool
prop_lengthPlusPlus xs ys = length (xs ++ ys) == length xs + length ys

{-
length ([] ++ ys) == length [] + length ys                   (base)

length ((x:xs) ++ ys) == length (x:xs) + length ys           (ind)

length (xs ++ ys) = length xs + length ys                    (hyp)

-- BASE CASE --
length ys == length [] + length ys       by (++.1)
length ys == 0  + length ys              by (length.1)
length ys == length ys

-- INDUCTION STEP --
length (x : (xs ++ ys)) == length (x:xs) + length ys      by (++.2)
length (x : (xs ++ ys)) == 1 + length xs + length ys      by (length.2)
  1 + length (xs ++ ys) == 1 + length xs + length ys      by (length.2)
      length (xs ++ ys) == length xs + length ys          by arith.
  length xs + length ys == length xs + length ys          by (hyp)
-}

-- 2)
reverse :: [a] -> [a]
reverse []     = []
reverse (z:zs) = reverse zs ++ [z]

prop_reversePlusPlus :: (Eq a) => [a] -> [a] -> Bool
prop_reversePlusPlus xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

{-

reverse ([] ++ ys)   == reverse ys ++ reverse []         (base)

reverse ((x:xs)++ys) == reverse ys ++ reverse (x:xs)     (ind)

reverse (xs++ys)     == reverse ys ++ reverse xs         (hyp)

-- BASE CASE --
reverse ys == reverse ys ++ reverse []                 by (++.1)
reverse ys == reverse ys ++ []                         by (reverse.1)

We need to prove that (xs ++ [] == xs)  (A) in order to continue.
    [] ++ []     == []             (base)

    (x:xs) ++ [] == (x:xs)         (ind)

    xs ++ []     == xs             (hyp)

    -- BASE CASE --
    [] == []                     by (++.1)

    -- INDUCTION STEP --
    (x:xs) ++ []   == (x:xs)
    x : (xs ++ []) == (x:xs)     by (++.2)
    x : xs         == (x:xs)     by (hyp)
    x : xs         == x : xs     by arith.

So we have that:
reverse ys == reverse ys         by (A)

-- INDUCTION STEP--
reverse ((x:xs) ++ ys)            == reverse ys ++ reverse (x:xs)
reverse (x : (xs ++ ys))          == reverse ys ++ reverse (x:xs)         by (++.2)
reverse (x : (xs++ys))            == reverse ys ++ reverse xs ++ [x]      by (reverse.2)
reverse (xs++ys) ++ [x]           == reverse ys ++ (reverse xs ++ [x])    by (reverse.2)
(reverse ys ++ reverse xs) ++ [x] == reverse ys ++ (reverse xs ++ [x])    by (hyp)

We need to prove that xs ++ (ys ++ zs) == (xs ++ ys) ++ zs  (B) in order to continue.
    [] ++ (ys ++ zs)     == ([] ++ ys) ++ zs       (base)
    (x:xs) ++ (ys ++ zs) == ((x:xs) ++ ys) ++ zs   (ind)
    xs ++ (ys ++ zs)     == (xs ++ ys) ++ zs       (hyp)

    -- BASE CASE --
    (ys ++ zs) == ([] ++ ys) ++ zs      by (++.1)
      ys ++ zs == ys ++ zs              by (++.1)

    -- INDUCTION STEP --
    x : xs ++ (ys ++ zs)  == x : (xs ++ ys) ++ zs    by (++.2)
    x : (xs ++ ys) ++ zs  == x : (xs ++ ys) ++ zs    by (hyp)

So we have that:
reverse ys ++ (reverse xs ++ [x]) == reverse ys ++ (reverse xs ++ [x])    by (B)

-}

-- 9.5)
prop_sumPlusPlus :: (Num a, Eq a) => [a] -> [a] -> Bool
prop_sumPlusPlus xs ys = sum (xs ++ ys) == sum xs + sum ys

-- Goal: Prove that "sum (xs ++ ys) = sum xs + sum ys" by induction over xs

{-
    sum ([] ++ ys)     == sum [] + sum ys         (base)
    sum ((x:xs) ++ ys) == sum (x:xs) + sum ys     (ind)
    sum (xs ++ ys)     == sum xs + sum ys         (hyp)

    -- BASE CASE --
    sum ys == sum [] + sum ys             by (++.1)
    sum ys == 0 + sum ys                  by (sum.1)
    sum ys == sum ys                      by arith.

    -- INDUCTION STEP --
    sum (x : (xs++ys))    == sum (x:xs) + sum ys      by (++.2)
    x + sum (xs ++ ys)    == sum (x:xs) + sum ys      by (sum.2)
    x + sum (xs ++ ys)    == (x + sum xs) + sum ys    by (sum.2)
    x + (sum xs + sum ys) == (x + sum xs) + sum ys    by (hyp)
    x + sum xs + sum ys   == x + sum xs + sum ys      by arith.
-}

-- 9.6)
-- Already done (refer to "PreludeProofs.hs")

-- 9.7)
prop_sumReverse :: (Num a, Eq a) => [a] -> Bool
prop_sumReverse xs = sum (reverse xs) == sum xs

prop_lengthReverse :: [a] -> Bool
prop_lengthReverse xs = length (reverse xs) == length xs

-- Goal: Prove that "sum (reverse xs) == sum xs" by induction over xs
{-
    sum (reverse [])     == sum []           (base)
    sum (reverse (x:xs)) == sum (x:xs)       (ind)
    sum (reverse xs)     == sum xs           (hyp)

    -- BASE CASE --
    sum [] == sum []      by (reverse.1)

    -- INDUCTION STEP --
    sum (reverse xs ++ [x])     == sum (x:xs)       by (reverse.2)
    sum (reverse xs ++ [x])     == x + sum xs       by (sum.2)
    sum (reverse xs) + sum [x]) == x + sum xs       by (prop_sumPlusPlus)
    sum xs + sum [x]            == x + sum xs       by (hyp)
    sum xs + sum (x:[])         == x + sum xs       by (list notation)
    sum xs + (x + sum [])       == x + sum xs       by (sum.2)
    sum xs + (x + 0)            == x + sum xs       by (sum.1)
    sum xs + x                  == x + sum xs       by arith.
    x + sum xs                  == x + sum xs       by arith.
-}

-- Goal: Prove that "length (reverse xs) == length xs" by induction over xs
{-
    length (reverse [])     == length []          (base)
    length (reverse (x:xs)) == length (x:xs)      (ind)
    length (reverse xs)     == length xs          (hyp)

    -- BASE CASE --
    length [] == length []      by (reverse.1)

    -- INDUCTION STEP --
    length (reverse xs ++ [x])       == length (x:xs)       by (reverse.2)
    length (reverse xs ++ [x])       == 1 + length xs       by (length.2)
    length (reverse xs) + length [x] == 1 + length xs       by (prop_lengthPlusPlus)
    length xs + length [x]           == 1 + length xs       by (hyp)
    length xs + 1                    == 1 + length xs       by (length)
    1 + length xs                    == 1 + length xs       by artith.
-}

-- 9.8)
elem :: (Eq a) => a -> [a] -> Bool
elem _ []     = False
elem z (x:xs) = z == x || elem z xs

prop_elemOr :: (Eq a) => a -> [a] -> [a] -> Bool
prop_elemOr z xs ys = elem z (xs ++ ys) == elem z xs || elem z ys

-- Goal: Prove that "elem z (xs ++ ys) == elem z xs || elem z ys" by induction over xs

{-
    elem z ([] ++ ys)     == elem z [] || elem z ys         (base)
    elem z ((x:xs) ++ ys) == elem z (x:xs) || elem z ys     (ind)
    elem z (xs ++ ys)     == elem z xs || elem z ys         (hyp)

    -- BASE CASE --
    elem z ys == elem z [] || elem z ys       by (++.1)
    elem z ys == False || elem z ys           by (elem.1)
    elem z ys == elem z ys                    by boolean_arith.

    -- INDUCTION STEP --
    elem z (x : (xs ++ ys))          == elem z (x:xs) || elem z ys             by (++.2)
    elem z (x : (xs ++ ys))          == (z==x || elem z xs) || elem z ys       by (elem.2)
    z==x || elem z (xs ++ ys)        == (z==x || elem z xs) || elem z ys       by (elem.2)
    z==x || (elem z xs || elem z ys) == (z==x || elem z xs) || elem z ys       by (hyp)
    z==x || elem z xs || elem z ys   == z==x || elem z xs || elem z ys         by bool_arith.
-}

-- 9.9)
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _           = []

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x,y):ps) = (x:xs, y:ys)
    where
        (xs, ys) = unzip ps

prop_Zip :: (Eq a, Eq b) => [(a, b)] -> Bool
prop_Zip ps = zip (fst (unzip ps)) (snd (unzip ps)) == ps

-- Goal: Prove that "zip (fst (unzip ps)) (snd (unzip ps)) == ps" by induction over ps

{-
    zip (fst (unzip [])) (snd (unzip []))         == []         (base)
    zip (fst (unzip (p:ps))) (snd (unzip (p:ps))) == (p:ps)     (ind)
    zip (fst (unzip ps)) (snd (unzip ps))         == ps         (hyp)

    -- BASE CASE --
    zip (fst ([],[])) (snd ([],[]))  == []       by (unzip.1)
               zip [] (snd ([],[]))  == []       by (fst)
                          zip [] []  == []       by (snd)
                                 []  == []       by (zip.2)

    -- INDUCTION STEP --
    unzip (p:ps) == unzip ((px,py) : ps)                            by pair_notation
                 == (px : fst (unzip ps), py : snd (unzip ps))      by (unzip.2)

    fst (unzip (p:ps)) = px : fst (unzip ps)
    snd (unzip (p:ps)) = py : snd (unzip ps)

    zip (fst (unzip (p:ps))) (snd (unzip (p:ps)))   == (p:ps)       (ind)
    zip (px : fst (unzip ps)) (py : snd (unzip ps)) == (p:ps)
    (px,py) : zip (fst (unzip ps)) (snd (unzip ps)) == (p:ps)       by (zip.1)
          p : zip (fst (unzip ps)) (snd (unzip ps)) == (p:ps)       by pair_notation
                                               p:ps == (p:ps)       by (hyp)

-}

prop_unzipZip :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_unzipZip xs ys = unzip (zip xs ys) == (xs,ys) || length xs /= length ys

-- Goal: Prove that "unzip (zip xs ys) == (xs,ys) IFF length xs == length ys"
--       by induction over xs.

{-
 "<="
    CONSTRAINT: length xs == length ys

    unzip (zip [] ys)     == ([], ys)           (base)
    unzip (zip (x:xs) ys) == ((x:xs), ys)       (ind)
    unzip (zip xs ys)     == (xs, ys)           (hyp)

    -- BASE CASE --
    0 == length [] == length ys <=> ys == []

    unzip (zip [] []) == ([], [])       (base)
             unzip [] == ([], [])       by (zip.2)
              ([],[]) == ([], [])       by (unzip.1)

    -- INDUCTION STEP --
                                      unzip (zip (x:xs) (y:ys')) == ((x:xs), ys))      by list_notation
                                      unzip ((x,y) : zip xs ys') == ((x:xs), ys))      by (zip.1)
    (x : fst (unzip (zip xs ys')), y : snd (unzip (zip xs ys'))) == ((x:xs), ys)       by (unzip.2)
                          (x : fst (xs, ys'), y : snd (xs, ys')) == ((x:xs), ys)       by (hyp)
                                               (x : xs, y : ys') == ((x:xs), ys)       by (fst) && (snd)
                                                    (x : xs, ys) == (x : xs, ys)       by list_notation

 "=>"
    CONSTRAINT: unzip (zip xs ys) == (xs, ys)

    If length xs /= length ys, we can choose, for example, xs = [2] and ys = [1,3].
    Then unzip (zip [2] [1,3]) = unzip [(2,1)] = ([2], [1]) /= ([2], [1,3])  (CONTRADICTION!)
    Then, by necessity, length xs == length ys.
-}

-- 9.10)
take :: (Integral a) => a -> [b] -> [b]
take _ []       = []
take n (x:xs)
    | n <= 0    = []
    | otherwise = x : take (n-1) xs

drop :: (Integral a) => a -> [b] -> [b]
drop _ []       = []
drop n (x:xs)
    | n <= 0    = x:xs
    | otherwise = drop (n-1) xs

prop_takeDrop :: (Eq a) => Int -> [a] -> Bool
prop_takeDrop n xs = take n xs ++ drop n xs == xs

-- Goal: Prove that "take n xs ++ drop n xs == xs" by induction over xs.

{-
    take n [] ++ drop n []         == []           (base)
    take n (x:xs) ++ drop n (x:xs) == (x:xs)       (ind)
    take n xs ++ drop n xs         == xs           (hyp)

    -- BASE CASE --
    [] ++ drop n [] == []       by (take.1)
           [] ++ [] == []       by (drop.1)
                 [] == []       by (++.1)
    
    -- INDUCTION STEP --
    Case I) If n <= 0, then:
     [] ++ drop n (x:xs) == (x:xs)       by (take.2.G1)
            [] ++ (x:xs) == (x:xs)       by (drop.2.G1)
                  (x:xs) == (x:xs)       by (++.1)

    Case II) If n >= length xs, then:
     take n (x:xs) = [x,x1,...,xn-1] if n == length xs
                     (x:xs)          otherwise

     drop n (x:xs) = [xn] if n == length xs
                     []   otherwise

    Case III) If n > 0 && n < length xs = m
     take n [x,x1,...,xn,...,xm] ++ drop n [x,x1,...,xn,...,xm] =
     (x : take (n-1) [x1,...,xn,...,xm]) ++ drop (n-1) [x1,...,xn,...,xm] =
     (x : x1 : ... : xn-1 : take 0 [xn...xm]) ++ [xn..xm]
     (x : x1 : ... : xn-1 : []) ++ [xn...xm]
     [x,x1,...,xn-1] ++ [xn...xm] = [x,x1...,xn-1,xn,...xm]
-}
