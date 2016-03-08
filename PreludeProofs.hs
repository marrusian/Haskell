module PreludeProofs where
    prop_PlusPlus_RightEmptyList :: (Eq a) => [a] -> [a] -> Bool
    prop_PlusPlus_RightEmptyList xs [] = xs ++ [] == xs

{-
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
-}

    prop_PlusPlus_LeftEmptyList :: (Eq a) => [a] -> [a] -> Bool
    prop_PlusPlus_LeftEmptyList [] xs  = [] ++ xs == xs

{-
    [] ++ []     == []             (base)
    [] ++ (x:xs) == (x:xs)         (ind)
    [] ++ xs     == xs             (hyp)

    -- BASE CASE --
    [] == []                     by (++.1)

    -- INDUCTION STEP --
    [] ++ (x:xs)   == (x:xs)
    x : xs         == x : xs     by (++.1)
-}

    prop_PlusPlus_Associativity :: (Eq a) => [a] -> [a] -> [a] -> Bool
    prop_PlusPlus_Associativity xs ys zs = xs ++ (ys ++ zs) == (xs ++ ys) ++ zs

{-
    [] ++ (ys ++ zs)     == ([] ++ ys) ++ zs       (base)
    (x:xs) ++ (ys ++ zs) == ((x:xs) ++ ys) ++ zs   (ind)
    xs ++ (ys ++ zs)     == (xs ++ ys) ++ zs       (hyp)

    -- BASE CASE --
    (ys ++ zs) == ([] ++ ys) ++ zs      by (++.1)
      ys ++ zs == ys ++ zs              by (++.1)

    -- INDUCTION STEP --
    x : xs ++ (ys ++ zs)  == x : (xs ++ ys) ++ zs    by (++.2)
    x : (xs ++ ys) ++ zs  == x : (xs ++ ys) ++ zs    by (hyp)
-}

    prop_Length_PPAMorphism :: [a] -> [a] -> Bool
    prop_Length_PPAMorphism xs ys = length (xs ++ ys) == length xs + length ys

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

    prop_Length_Reverse :: [a] -> Bool
    prop_Length_Reverse xs = length (reverse xs) == length xs

{-
    length (reverse [])     == length []          (base)
    length (reverse (x:xs)) == length (x:xs)      (ind)
    length (reverse xs)     == length xs          (hyp)

    -- BASE CASE --
    length [] == length []      by (reverse.1)

    -- INDUCTION STEP --
    length (reverse xs ++ [x])       == length (x:xs)       by (reverse.2)
    length (reverse xs ++ [x])       == 1 + length xs       by (length.2)
    length (reverse xs) + length [x] == 1 + length xs       by (prop_Length_PPAMorphism)
    length xs + length [x]           == 1 + length xs       by (hyp)
    length xs + 1                    == 1 + length xs       by (length)
    1 + length xs                    == 1 + length xs       by arith.
-}

    prop_Reverse_PPAntiDistributivity :: [a] -> [a] -> Bool
    prop_Reverse_PPAntiDistributivity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

{-
    reverse ([] ++ ys)   == reverse ys ++ reverse []         (base)
    reverse ((x:xs)++ys) == reverse ys ++ reverse (x:xs)     (ind)
    reverse (xs++ys)     == reverse ys ++ reverse xs         (hyp)

    -- BASE CASE --
    reverse ys == reverse ys ++ reverse []                 by (++.1)
    reverse ys == reverse ys ++ []                         by (reverse.1)
    reverse ys == reverse ys                               by (prop_PlusPlus_RightEmptyList)

    -- INDUCTION STEP--
    reverse ((x:xs) ++ ys)            == reverse ys ++ reverse (x:xs)
    reverse (x : (xs ++ ys))          == reverse ys ++ reverse (x:xs)         by (++.2)
    reverse (x : (xs++ys))            == reverse ys ++ reverse xs ++ [x]      by (reverse.2)
    reverse (xs++ys) ++ [x]           == reverse ys ++ (reverse xs ++ [x])    by (reverse.2)
    (reverse ys ++ reverse xs) ++ [x] == reverse ys ++ (reverse xs ++ [x])    by (hyp)
    reverse ys ++ (reverse xs ++ [x]) == reverse ys ++ (reverse xs ++ [x])    by (prop_PlusPlus_Associativity)
-}

    prop_Sum_PPAMorphism :: (Num a, Eq a) => [a] -> [a] -> Bool
    prop_Sum_PPAMorphism xs ys = sum (xs ++ ys) == sum xs + sum ys

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

    prop_Sum_Reverse :: (Num a, Eq a) => [a] -> Bool
    prop_Sum_Reverse xs = sum (reverse xs) == sum xs

{-
    sum (reverse [])     == sum []           (base)
    sum (reverse (x:xs)) == sum (x:xs)       (ind)
    sum (reverse xs)     == sum xs           (hyp)

    -- BASE CASE --
    sum [] == sum []      by (reverse.1)

    -- INDUCTION STEP --
    sum (reverse xs ++ [x])     == sum (x:xs)       by (reverse.2)
    sum (reverse xs ++ [x])     == x + sum xs       by (sum.2)
    sum (reverse xs) + sum [x]) == x + sum xs       by (prop_Sum_PPAMorphism)
    sum xs + sum [x]            == x + sum xs       by (hyp)
    sum xs + sum (x:[])         == x + sum xs       by (list notation)
    sum xs + (x + sum [])       == x + sum xs       by (sum.2)
    sum xs + (x + 0)            == x + sum xs       by (sum.1)
    sum xs + x                  == x + sum xs       by arith.
    x + sum xs                  == x + sum xs       by arith.
-}

    prop_Elem_PPORMorphism :: (Eq a) => a -> [a] -> [a] -> Bool
    prop_Elem_PPORMorphism z xs ys = elem z (xs ++ ys) == elem z xs || elem z ys

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

    prop_Zip_UnzipInverse ::(Eq a, Eq b) => [(a, b)] -> Bool
    prop_Zip_UnzipInverse ps = zip (fst (unzip ps)) (snd (unzip ps)) == ps

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

    prop_Unzip_ZipInverse :: (Eq a, Eq b) => [a] -> [b] -> Bool
    prop_Unzip_ZipInverse xs ys = unzip (zip xs ys) == (xs,ys) || length xs /= length ys

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

      Let's assume that length xs /= length ys. We can choose, for example,
    length xs = m, length ys = n, where m /= n.

      Case I) If m > n, then:
       unzip (zip [x1,x2,...,xn,...,xm] [y1,y2...,yn]) =
                unzip [(x1,y1), (x2,y2), ..., (xn,yn)] =
                     ([x1,x2,...,xn], [y1,y2,...,yn]) /= ([x1,x2,...,xn,...,xm], [y1,y2,...,yn])    (CONTRADICTION!)
    
      Case II) If m < n then:
       unzip (zip [x1,x2,...,xn] [y1,y2,...,yn,...,ym]) =
                 unzip [(x1,y1), (x2,y2), ..., (xn,yn)] =
                      ([x1,x2,...,xn], [y1,y2,...,yn]) /= ([x1,x2,...,xn], [y1,y2,...,yn,...,ym])   (CONTRADICTION!)

      Therefore, we can deduce from Case I) && Case II) that, by necessity,
      length xs == length ys.
-}

    prop_Concat_PPDistributivity :: (Eq a) => [[a]] -> [[a]] -> Bool
    prop_Concat_PPDistributivity xs ys = concat (xs ++ ys) == concat xs ++ concat ys

{-
    By (PreludeProofs.prop_PlusPlus_Associativity), (++) is associative.

    By (PreludeProofs.prop_PlusPlus_RightEmptyList) &&
       (PreludeProofs.prop_PlusPlus_LeftEmptyList), [] is an identity for (++).

    By (foldr.++) we have that:
       foldr (++) [] (xs++ys) == (++) (foldr (++) [] xs) (foldr (++) [] ys)
                              == (foldr (++) [] xs) ++ (foldr (++) [] ys)

    Using the fact that "concat = foldr (++) []" results:
        concat (xs ++ ys) == concat xs ++ concat ys
-}