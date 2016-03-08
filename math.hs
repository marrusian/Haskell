    module Math where

    -- https://en.wikipedia.org/wiki/Machine_epsilon
    {-
        Approximation of the M.E:
            epsilon = 1.0;

            while (1.0 + 0.5 * epsilon) ≠ 1.0:
                epsilon = 0.5 * epsilon
    -}
    mEps :: (Fractional a, Eq a) => a
    mEps = findMachineEpsilon 1.0

    findMachineEpsilon:: (Fractional a, Eq a) => a -> a
    findMachineEpsilon eps
        | 1.0 + 0.5 * eps /= 1.0 = findMachineEpsilon (0.5 * eps)
        | otherwise              = eps

    -- https://wiki.haskell.org/Functional_differentiation
    {-
          Approximate the derivative f'(x) by \frac{f(x+h)-f(x)}{h} where h is
      close to zero (or at best the square root of the machine precision).
    -}
    derivative :: (Floating a, Eq a) => (a -> a) -> (a -> a)
    derivative f x = (f (x+h) - f x) / h
        where
            h = sqrt mEps

    -- https://www.math.ucdavis.edu/~kouba/CalcTwoDIRECTORY/defintdirectory/DefInt.html
    norm :: (Fractional a, Integral i) => a -> a -> i -> a
    norm a b n = (b - a) / fromIntegral n

    samplingPoints :: (Fractional a, Integral i) => a -> a -> i -> [a]
    samplingPoints a b n = [a + mesh * (fromIntegral i) | i <- [1..n]]
        where
            mesh = norm a b n

    integrate :: (Fractional a) => (a -> a) -> a -> a -> a
    integrate f a b = sum $ map ((*mesh) . f) ci
        where
            ci   = samplingPoints a b n
            mesh = norm a b n
            n    = 10^6
