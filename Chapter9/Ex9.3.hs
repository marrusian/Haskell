fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)

-- 9.1)
-- Evaluating: (4>2) || (fact (-1) == 17) => True
--   Explanation: Haskell is lazy, so given the behaviour of the "||" operator,
-- because (4>2) evaluates to "True", the right-hand term isn't needed in order
-- to compute the final result.

-- Evaluating: (4 > 2) && (fact (-1) == 17) => Undefined
--   Explanation: Unlike the expression above, the "&&" operator also needs the
-- value of the right-hand term. Because "fact (-1)" is undefined, the entire
-- expression becomes undefined.

-- 9.2)
mult :: (Integral a) => a -> a -> a
mult 0 _ = 0
-- mult _ 0 = 0 - Useless pattern match
mult n m = n*m

-- Evaluating: mult (fact (-2)) 0 => Undefined
--   Explanation: "mult" first evaluates the first argument and compares it to 0.
-- That is why "mult 0 (fact (-2))" evaluates to 0, because the second argument is
-- never evaluated (so it doesn't matter if it is undefined). However, when the
-- first argument is undefined, the entire expression becomes undefined.
