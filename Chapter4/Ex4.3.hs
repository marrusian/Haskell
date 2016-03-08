import Test.QuickCheck hiding (Result)

data Move = Rock | Paper | Scissors
            deriving (Show, Eq)

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock 
lose _     = Paper

-- 4.11)
data Result = Win | Lose | Draw
              deriving (Show, Eq)

-- 4.12)
outcome :: Move -> Move -> Result
outcome m n
    | lose m == n = Win
    | beat m == n = Lose
    | otherwise   = Draw

-- Properties --

-- 1) A move IS NOT idempotent

prop_Beat :: Move -> Bool
prop_Beat m = beat (beat m) /= m

prop_Lose :: Move -> Bool
prop_Lose m = lose (lose m) /= m

-- 2) (m.m.m)(x) = x [i.e., the order of the group is 3]

prop_Beat2 :: Move -> Bool
prop_Beat2 m = (beat.beat.beat) m == m

prop_Lose2 :: Move -> Bool
prop_Lose2 m = (lose.lose.lose) m == m

-- 3) Beat() and Lose() are the inverses of each other

prop_Beat3 :: Move -> Bool
prop_Beat3 m = (beat.lose) m == m

prop_Lose3 :: Move -> Bool
prop_Lose3 m = (lose.beat) m == m

-- -- 4.13) [connecting Beat() to Lose()]
prop_Move1 :: Move -> Bool
prop_Move1 m = beat (lose m) == lose (beat m)

-- 4.14)
prop_Outcome :: Move -> Move -> Bool
prop_Outcome m n
    | m == n    = (outcome m n == Draw)
    | otherwise = check
    where
      check
        | beat m == n = (outcome m n == Lose)
        | otherwise   = (outcome m n == Win)

-- 4.15)
data Season = Summer | Autumn | Winter | Spring
              deriving (Show, Eq, Ord)

data Temp = Cold | Hot
            deriving (Show, Eq, Ord)

seasonToTemp :: Season -> Temp
seasonToTemp s
  | s `elem` [Summer, Spring] = Hot
  | otherwise                 = Cold

-- 4.16)
data Month = January | February | March | April |
             May | June | July | August | September |
             October | November | December
             deriving (Show, Eq, Ord)

monthToSeason :: Month -> Season -- [Assuming northern hemisphere]
monthToSeason m
  | m `elem` [March, April, May]            = Spring
  | m `elem` [June, July, August]           = Summer
  | m `elem` [September, October, November] = Autumn
  | otherwise                               = Winter