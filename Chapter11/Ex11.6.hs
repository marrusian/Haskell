module Ex116 where
    import Prelude hiding (flip, map, (.), filter, reverse, (++), foldr, concat)
    import qualified Prelude
    import Test.QuickCheck

    -- 11.25)
    -- Goal: Prove that "f . (g . h) == (f . g) . h".
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

    -- 11.26)
    -- Goal: Prove that "id . f == f" forAll f.
    {-
        Let f be an arbitrary function. Let x be an arbitrary argument. Then:

        (id . f) x == id (f x)       by (comp_definition)
                   == f x            by (id_definition)

        Therefore: (id . f) == f forAll f    by (extensionality)
    -}

    -- 11.27)
    flip :: (a -> b -> c) -> (b -> a -> c)
    flip f x y = f y x

    -- Goal: Prove that "flip . flip == id".
    {-
        We will prove that "flip (flip f) == f" forAll f.
        Let f be an arbitrary function. Let x, y be two arbitrary arguments. Then:

        flip (flip f x y) == flip (f y x)       by (flip)
                          == f x y              by (flip)

        Therefore: (flip . flip) f == f forAll f      by (extensionality) 
        Therefore:  flip . flip    == id              by (extensionality)
    -}

    -- 11.28) INVERSES
    -- Goal: Prove that "curry . uncurry == id && uncurry . curry == id".
    {-
        We will prove that "curry (uncurry f) == f" forAll f.
        Let f be an arbitrary function. Let x, y be two arbitrary arguments. Then:

    I)
        curry (uncurry f (x,y)) == curry (f x y)       by (uncurry)
                                == f (x,y)             by (cirry)

        Therefore: (curry . uncurry) f == f forAll f     by (extensionality)
        Therefore:  curry . uncurry    == id             by (extensionality)

    II)
        uncurry (curry f x y)  == uncurry (f (x,y))     by (curry)
                               == f x y                 by (uncurry)

        Therefore: (uncurry . curry) f == f forAll f    by (extensionality)
        Therefore:  uncurry . curry    == id            by (extensionality)
    -}

    -- 11.29)
    iter :: (Integral i) => i -> (a -> a) -> (a -> a)
    iter n f
        | n > 0     = f . iter (n-1) f
        | otherwise = id

    -- Goal: Prove that "iter n id == id" by induction over n.
    {-
        iter   0   id == id      (base)
        iter (n+1) id == id      (ind)
        iter   n   id == id      (hyp)

        -- BASE CASE --
        iter 0 id == id          by (iter.2)

        -- INDUCTION STEP --
        id . iter n id == id     by (iter.1)
               id . id == id     by (hyp)
                    id == id     by (id . f == f forAll f)
    -}

    -- 11.30) IDEMPOTENCY
    -- Goal: Prove that "abs . abs == abs" && "signum . signum == signum".
    {-
        Let x be an arbitrary argument. Then:

        (abs . abs) x == abs (abs x)     by (comp_definition)

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

------------------------------------------------------------------------
    -- map f (xs ++ ys) == map f xs ++ map f ys   forAll f

    {-
        If f is associative, and st is an identity for f, that is:
            x `f` (y `f` z) == (x `f` y) `f` z
            x `f` st == x == st `f` x

        Then:
            foldr f st (xs ++ ys) == f (foldr f st xs) (foldr f st ys)    forAll x, y, z
    -}

    map :: (a -> b) -> [a] -> [b]
    map f []     = []
    map f (x:xs) = f x : map f xs

    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    (.) f g x = f (g x)

    -- Goal: Prove that "map (f.g) xs == (map f . map g) xs" by induction over xs.
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

    -- Properties can:
    --   1) State how the program behaves
    --   2) Relate one program to another (program transformation for FL)

    --   One of the most useful of the basic functional transformations
    -- is called "FILTER PROMOTION", which states that:
    --      filter p . map f == map f . filter (p . f)

    filter :: (a -> Bool) -> [a] -> [a]
    filter p []     = []
    filter p (x:xs)
        | p x       = x : filter p xs
        | otherwise =     filter p xs

    -- Goal: Prove that "(filter p . map f) xs == (map f . filter (p . f)) xs" by induction over xs.
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

    prop_FilterPromotion :: (Eq b) => (b -> Bool) -> (a -> b) -> [a] -> Bool
    prop_FilterPromotion p f xs = (filter p . map f $ xs) == (map f . filter (p.f) $ xs)

    -- quickCheck $ prop_FilterPromotion (>0)  (\x -> x*x)
    -- quickCheck $ prop_FilterPromotion (>=0) (\x -> x*x*x)

    reverse :: [a] -> [a]
    reverse []     = []
    reverse (x:xs) = reverse xs ++ [x]

    (++) :: [a] -> [a] -> [a]
    (++)   []   ys = ys
    (++) (x:xs) ys = x : xs ++ ys

    -- Goal: Prove that "map f (xs ++ ys) == map f xs ++ map f ys" forAll f
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

    -- Goal: Prove that "map f (reverse xs) = reverse (map f xs)" by induction over xs
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

    -- 11.31) See above

    -- 11.32)
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f st []     = st
    foldr f st (x:xs) = x `f` (foldr f st xs)

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

    -- 11.33)
    -- Goal: Prove that "concat (xs ++ ys) = concat xs ++ concat ys" by
    --       using the fact that "concat = foldr (++) []"
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

    -- 11.34)
    concat :: [[a]] -> [a]
    concat []     = []
    concat (x:xs) = x ++ concat xs

    -- Goal: concat (map (map f) xs) = map f (concat xs)
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

    -- 11.35)
    -- Goal: Prove that "(0<) . (+1) = (0 <=)"
    {-
        Let x be an arbitrary argument. Then:
        ((0<) . (+1)) x == (0<) (x+1)      by (comp.1)
                        == (0 < x+1)       by (operator section)
                        == (-1 < x)        by arith.
                        == (0 <= x)        by arith.

        Therefore: "(0<) . (+1) = (0 <=)"      by (extensionality)
    -}

    -- 11.36)
    infixr 3 &&&
    (&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    (&&&) p q x = p x && q x

    -- Goal: Prove that "filter p (filter q xs) = filter (p &&& q) xs" by induction over xs.
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

