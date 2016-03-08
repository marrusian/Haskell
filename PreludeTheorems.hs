module PreludeTheorems where
    import Test.QuickCheck

    -- HELPER FUNCTIONS --
    -- f is associative if: "x `f` (y `f` z) == (x `f` y) `f` z"
    isAssociative :: (Eq a) => (a -> a -> a) -> (a -> a -> a -> Bool)
    isAssociative f = (\x y z -> x `f` (y `f` z) == (x `f` y) `f` z)

    -- x is an identity for f if: "x `f` y == y == y `f` x"
    isIdentity :: (Eq a) => (a -> a -> a) -> a -> (a -> Bool)
    isIdentity f x = (\y -> f x y == f y x && f y x == y)

    -- THEOREMS --
    prop_Comp_Associativity :: (Eq d) => (c -> d) -> (b -> c) -> (a -> b) -> a -> Bool
    prop_Comp_Associativity f g h x = (f . (g . h)) x == ((f . g) . h) x

{-
    Let x be an arbitrary argument. Then:

    (f . (g . h)) x ==  (f . p) x        by (notation (g . h) = p)
                    ==  f (p x)          by (comp_definition)
                    ==  f ((g . h) x)    by (expanding p)
                    ==  f (g (h x))      by (comp_definition)

    ((f . g) . h) x == (p . h) x         by (notation (f . g) = p)
                    == p (h x)           by (comp_definition)
                    == (f . g) (h x)     by (expanding p)
                    == f (g (h x))       by (comp_definition)

    Therefore: f . (g . h) == (f . g) . h      by (extensionality)
-}

    prop_Comp_LeftIdentity :: (Eq b) => (a -> b) -> a -> Bool
    prop_Comp_LeftIdentity f x = (id . f) x == f x
    
{-
    Let f be an arbitrary function. Let x be an arbitrary argument. Then:

    (id . f) x == id (f x)       by (comp_definition)
               == f x            by (id_definition)

    Therefore: (id . f) == f forAll f      by (extensionality)
-}

    prop_Comp_RightIdentity :: (Eq b) => (a -> b) -> a -> Bool
    prop_Comp_RightIdentity f x = (f . id) x == f x

{-
    Let f be an arbitrary function. Let x be an arbitrary argument. Then:

    (f . id) x == f (id x)       by (comp.1)
               == f x            by (id)

    Therefore: (f . id) == f forAll f      by (extensionality)
-}

    prop_Flip_FlipInverse :: (Eq c) => (a -> b -> c) -> a -> b -> Bool
    prop_Flip_FlipInverse f x y = (flip . flip) f x y == f x y

{-
    We will prove that "flip (flip f) == f" forAll f.
    Let f be an arbitrary function. Let x, y be two arbitrary arguments. Then:

    flip (flip f x y) == flip (f y x)       by (flip)
                      == f x y              by (flip)

    Therefore: (flip . flip) f == f forAll f      by (extensionality) 
    Therefore:  flip . flip    == id              by (extensionality)
-}

    prop_Curry_UncurryInverse :: (Eq c) => (a -> b -> c) -> a -> b -> Bool
    prop_Curry_UncurryInverse f x y = (curry . uncurry) f x y == f x y

{-
    We will prove that "curry (uncurry f) == f" forAll f.
    Let f be an arbitrary function. Let x, y be two arbitrary arguments. Then:

    curry (uncurry f (x,y)) == curry (f x y)       by (uncurry)
                                == f (x,y)             by (cirry)

    Therefore: (curry . uncurry) f == f forAll f     by (extensionality)
    Therefore:  curry . uncurry    == id             by (extensionality)
-}

    prop_Uncurry_CurryInverse :: (Eq c) => ((a,b) -> c) -> a -> b -> Bool
    prop_Uncurry_CurryInverse f x y = (uncurry . curry) f (x,y) == f (x,y)

{-
    We will prove that "curry (uncurry f) == f" forAll f.
    Let f be an arbitrary function. Let x, y be two arbitrary arguments. Then:

    uncurry (curry f x y)  == uncurry (f (x,y))     by (curry)
                               == f x y                 by (uncurry)

    Therefore: (uncurry . curry) f == f forAll f    by (extensionality)
    Therefore:  uncurry . curry    == id            by (extensionality)
-}

    prop_Abs_Idempotency :: (Num a, Eq a) => a -> Bool
    prop_Abs_Idempotency x = (abs . abs) x == abs x

{-
    Let x be an arbitrary argument. Then:

    abs . abs) x == abs (abs x)     by (comp_definition)

    Case I) If x >= 0, then:
        abs (abs x) == abs x         by (abs)
                    == x             by (abs)

                abs x == x             by (abs)

        Therefore: (abs . abs) x == abs x    (x >= 0)

    Case II) If x < 0, then:
        abs (abs x) == abs (-x)      by (abs)
                    == -x            by (abs)

                abs x == -x            by (abs)

        Therefore: (abs . abs) x == abs x    (x < 0)

    By Case I) && Case II) we have that:
        (abs . abs) x == abs x forAll x

    Therefore: abs . abs == abs       by (extensionality)
-}

    prop_Signum_Idempotency :: (Num a, Eq a) => a -> Bool
    prop_Signum_Idempotency x = (signum . signum) x == signum x

{-
    Let x be an arbitrary argument. Then:

    (signum . signum) x == signum (signum x)     by (comp_definition)

    Case I) If x > 0, then:
        signum (signum x) == signum 1       by (signum)
                            == 1              by (signum)

                signum x  == 1              by (signum)

        Therefore: (signum . signum) x == signum x    (x > 0)

    Case II) If x == 0, then:
        signum (signum 0) == signum 0       by (signum)
                            == 0              by (signum)

                signum 0  == 0              by (signum)

        Therefore: (signum . signum) 0 == signum 0

    Case III) If x < 0, then:
        signum (signum x) == signum (-1)    by (signum)
                            == (-1)           by (signum)

                signum x  == (-1)           by (signum)

        Therefore: (signum . signum) x == signum x    (x < 0)

    By Case I) && Case II) && Case III) we have that:
        (signum . signum) x == signum x forAll x

    Therefore: signum . signum == signum      by (extensionality)
-}

    prop_Map_PPDistributivity :: (Eq b) => (a -> b) -> [a] -> [a] -> Bool
    prop_Map_PPDistributivity f xs ys  = map f (xs ++ ys) == map f xs ++ map f ys

{-
    map f ([] ++ ys)     == map f [] ++ map f ys        (base)
    map f ((x:xs) ++ ys) == map f (x:xs) ++ map f ys    (ind)
    map f (xs ++ ys)     == map f xs ++ map f ys        (hyp)

     -- BASE CASE --
    map f ys == map f [] ++ map f ys      by (++.1)
    map f ys == [] ++ map f ys            by (map.1)
    map f ys == map f ys                  by (++.1)

    -- INDUCTION STEP --
    map f (x : xs ++ ys)       == map f (x:xs) ++ map f ys       by (++.2)
    f x : map f (xs ++ ys)     == map f (x:xs) ++ map f ys       by (map.2)
    f x : map f (xs ++ ys)     == (f x : map f xs) ++ map f ys   by (map.2)
    f x : map f xs ++ map f ys == f x : map f xs ++ map f ys     by (hyp)
-}

    prop_Map_CompDistributivity :: (Eq c) => (b -> c) -> (a -> b) -> [a] -> Bool
    prop_Map_CompDistributivity f g xs = map (f.g) xs == (map f . map g) xs

{-
    map (f.g) []     == (map f . map g) []        (base)
    map (f.g) (x:xs) == (map f . map g) (x:xs)    (ind)
    map (f.g) xs     == (map f . map g) xs        (hyp)

    -- BASE CASE --
    [] == (map f . map g) []       by (map.1)
    [] == map f (map g [])         by (comp.1)
    [] == map f []                 by (map.1)
    [] == []                       by (map.1)

    -- INDUCTION STEP --
    (f.g) x : map (f.g) xs       == (map f . map g) (x:xs)        by (map.2)
    (f.g) x : map (f.g) xs       == map f (map g (x:xs))          by (comp.1)
    (f.g) x : map (f.g) xs       == map f (g x : map g xs)        by (map.2)
    (f.g) x : map (f.g) xs       == f (g x) : map f (map g xs)    by (map.2)
    (f.g) x : (map f . map g) xs == f (g x) : map f (map g xs)    by (hyp)
    f (g x) : (map f . map g) xs == f (g x) : map f (map g xs)    by (comp.1)
    f (g x) : map f (map g xs)   == f (g x) : map f (map g xs)    by (comp.1)
-}

    prop_Map_FilterPromotion :: (Eq b) => (a -> b) -> (b -> Bool) -> [a] -> Bool
    prop_Map_FilterPromotion f p xs = (filter p . map f) xs == (map f . filter (p.f)) xs

{-
    (filter p . map f) []     == (map f . filter (p.f)) []        (base)
    (filter p . map f) (x:xs) == (map f . filter (p.f)) (x:xs)    (ind)
    (filter p . map f) xs     == (map f . filter (p.f)) xs        (hyp)

    -- BASE CASE --
    filter p (map f []) == (map f . filter (p.f)) []       by (comp.1)
    filter p (map f []) == map f (filter (p.f) [])         by (comp.1)
    filter p []         == map f (filter (p.f) [])         by (map.1)
    filter p []         == map f []                        by (filter.1)
                 []         == map f []                        by (filter.1)
                 []         == []                              by (map.1)

    -- INDUCTION STEP --
    filter p (map f (x:xs))   == map f (filter (p.f) (x:xs))     by 2x(comp.1)
    filter p (f x : map f xs) == map f (filter (p.f) (x:xs))     by (map.2)
        
    Case I) If p (f x) == True, then:
        f x : filter p (map f xs)       == map f (filter (p.f) (x:xs))        by (filter.2.G1)
        f x : filter p (map f xs)       == map f (x : filter (p.f) xs)        by (filter.2.G1)
        f x : filter p (map f xs)       == f x : map f (filter (p.f) xs)      by (map.2)
        f x : (filter p . map f) xs     == f x : (map f . filter (p.f)) xs    by (comp.1)
        f x : (map f . filter (p.f)) xs == f x : (map f . filter (p.f)) xs    by (hyp)
        
    Case II) If p (f x) == False, then:
        filter p (map f xs)        == map f (filter (p.f) (x:xs))    by (filter.2.G2)
        filter p (map f xs)        == map f (filter (p.f) xs)        by (filter.2.G2)
        (filter p . map f) xs      == (map f . filter (p.f)) xs      by (comp.1)
        (map f . filter (p.f.)) xs == (map f . filter (p.f.)) xs     by (hyp)
-}

    prop_Map_ReverseComutativity :: (Eq b) => (a -> b) -> [a] -> Bool
    prop_Map_ReverseComutativity f xs = map f (reverse xs) == reverse (map f xs)

{-
    map f (reverse [])     == reverse (map f [])         (base)
    map f (reverse (x:xs)) == reverse (map f (x:xs))     (ind)
    map f (reverse xs)     == reverse (map f xs)         (hyp)

    -- BASE CASE --
    [] == reverse (map f [])      by (reverse.1)
    [] == reverse []              by (map.1)
    [] == []                      by (reverse.1)

    -- INDUCTION STEP --
    map f (reverse xs ++ [x])       == reverse (map f (x:xs))        by (reverse.2)
    map f (reverse xs ++ [x])       == reverse (f x : map f xs)      by (map.2)
    map f (reverse xs) ++ map f [x] == reverse (f x : map f xs)      by (map.++)
    map f (reverse xs) ++ map f [x] == reverse (map f xs) ++ [f x]   by (reverse.2)
    map f (reverse xs) ++ [f x]     == reverse (map f xs) ++ [f x]   by (map.2) && (map.1)
    reverse (map f xs) ++ [f x]     == reverse (map f xs) ++ [f x]   by (hyp)
-}

    prop_Foldr_PPDistributivity :: (Eq a) => (a -> a -> a) -> a -> [a] -> [a] -> a -> a -> a -> Bool
    prop_Foldr_PPDistributivity f st xs ys x y z = foldr f st (xs++ys) == f (foldr f st xs) (foldr f st ys) ||
                                                   ((not . isIdentity f st) x || (not . isAssociative f x y) z)
 
-- Goal: If f is associative, and st is an identity for f, prove that
--      "foldr f st (xs++ys) = f (foldr f st xs) (foldr f st ys)"
{-
    foldr f st ([] ++ ys)     == f (foldr f st []) (foldr f st ys)        (base)
    foldr f st ((x:xs) ++ ys) == f (foldr f st (x:xs)) (foldr f st ys)    (ind)
    foldr f st (xs ++ ys)     == f (foldr f st xs) (foldr f st ys)        (hyp)

    -- BASE CASE --
    foldr f st ys == f (foldr f st []) (foldr f st ys)      by (++.1)
    foldr f st ys == f (foldr f st []) (foldr f st ys)      by (foldr.1)
    foldr f st ys == f st (foldr f st ys)                   by (foldr.1)
    foldr f st ys == foldr f st ys                          by (st identity)

    -- INDUCTION STEP --
    foldr f st (x : (xs++ys)) == f (foldr f st (x:xs)) (foldr f st ys)     by (++.2)
    f x (foldr f st (xs++ys)) == f (foldr f st (x:xs)) (foldr f st ys)     by (foldr.2)
    f x (foldr f st (xs++ys)) == f (f x (foldr f st xs)) (foldr f st ys)   by (foldr.2)
    f x (foldr f st (xs++ys)) == f x (f (foldr f st xs) (foldr f st ys))   by (f associative)
    f x (f (foldr f st xs) (foldr f st ys)) == f x (f (foldr f st xs) (foldr f st ys))    by (hyp)
-}

    prop_Map_Concat :: (Eq b) => (a -> b) -> [[a]] -> Bool
    prop_Map_Concat f xs = concat (map (map f) xs) == map f (concat xs)

{-
    concat (map (map f) [])     == map f (concat [])         (base)
    concat (map (map f) (x:xs)) == map f (concat (x:xs))     (ind)
    concat (map (map f) xs)     == map f (concat xs)         (hyp)

    We could make the notation g = map f
    -- BASE CASE --
    concat [] == map f (concat [])      by (map.1)
    concat [] == map f []               by (concat.1)
               [] == map f []               by (concat.1)
               [] == []                     by (map.1)

    -- INDUCTION STEP --
    concat ((map f) x : map (map f) xs)  == map f (concat (x:xs))      by (map.2)
    concat ((map f) x : map (map f) xs)  == map f (x ++ concat xs)     by (concat.2)
    (map f) x ++ concat (map (map f) xs) == map f (x ++ concat xs)     by (concat.2)
    (map f) x ++ map f (concat xs)       == map f (x ++ concat xs)     by (hyp)
    map f (x ++ concat xs)               == map f (x ++ concat xs)     by (map.++)
-}

    infixr 3 &&&
    (&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    (&&&) p q x = p x && q x

    -- (filter p . filter q) == filter (p && q)
    prop_Filter_AndCompMorphism :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> Bool
    prop_Filter_AndCompMorphism p q xs = (filter p . filter q) xs == filter (p &&& q) xs

{-
    filter p (filter q [])     == filter (p &&& q) []         (base)
    filter p (filter q (x:xs)) == filter (p &&& q) (x:xs)     (ind)
    filter p (filter q xs)     == filter (p &&& q) xs         (hyp)

    -- BASE CASE --
    filter p [] == filter (p &&& q) []      (filter.1)
    filter p [] == []                       (filter.1)
                 [] == []                       (filter.1)

    -- INDUCTION STEP --
    Case I) If q x == True && p x == True [p &&& q $ x == True]

        filter p (x : filter q xs) == filter (p &&& q) (x:xs)    by (filter.2)
        x : filter p (filter q xs) == filter (p &&& q) (x:xs)    by (filter.2)
        x : filter p (filter q xs) == x : filter (p &&& q) xs    by (filter.2), (&&&.1)
        x : filter (p &&& q) xs    == x : filter (p &&& q) xs    by (hyp)

    Case II) If q x == True && p x == False [p &&& q $ x == False]

        filter p (x : filter q xs) == filter (p &&& q) (x:xs)    by (filter.2)
        filter p (filter q xs)     == filter (p &&& q) (x:xs)    by (filter.2)
        filter p (filter q xs)     == filter (p &&& q) xs        by (filter.2), (&&&.1)
        filter (p &&& q) xs        == filter (p &&& q) xs        by (hyp)

    Case III) If q x == False && (p x == True || p x == False) [p &&& q $ x == False]

        filter p (filter q xs) == filter (p &&& q) (x:xs)      by (filter.2)
        filter p (filter q xs) == filter (p &&& q) xs          by (filter.2), (&&&.1)
        filter (p &&& q) xs    == filter (p &&& q) xs          by (hyp)
-}
