module RPS where

import System.IO.Unsafe
import Data.Time

data Move = Rock | Paper | Scissors
            deriving (Show, Eq)

moves :: [Move]
moves = [Rock, Paper, Scissors]

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat _        = Rock

lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock 
lose _     = Paper

data Result = Win | Lose | Draw
              deriving (Show, Eq)

-- Invariant: (length . fst) == (length . snd)
type Tournament = ([Move], [Move])

-- 8.1)
outcome :: (Integral a) => Move -> Move -> a
outcome m n
    | lose m == n = 1
    | beat m == n = -1
    | otherwise   = 0

-- 8.2)
tournamentOutcome :: (Integral a) => Tournament -> a
tournamentOutcome (msA, msB) = score
    where
        score = sum [outcome (msA !! i) (msB !! i) | i <- [0..length msA - 1]]

tournamentOutcome' :: (Integral a) => Tournament -> a
tournamentOutcome' = sum . map (uncurry outcome) . uncurry zip

type Strategy = [Move] -> Move

rock, paper, scissors :: Strategy
rock _     = Rock
paper _    = Paper
scissors _ = Scissors

cycleStrategy :: Strategy
cycleStrategy ms = case (length ms) `rem` 3 of
                    0 -> Rock
                    1 -> Paper
                    2 -> Scissors
-------------------------------------------------------------------
--
-- Random stuff from time
--

-- Generate a random integer within the IO monad.

randomInt :: Integer -> IO Integer

randomInt n = 
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

-- Extract the random number from the IO monad, unsafely!

randInt :: Integer -> Integer

randInt = unsafePerformIO . randomInt 

convertToMove :: Integer -> Move

convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors

randomStrategy :: Strategy
randomStrategy _ = convertToMove $ randInt 3
-------------------------------------------------------------------

echoStrategy :: Strategy
echoStrategy (latest:rest) = latest
echoStrategy _             = Rock

-- 8.3)
win_previousStrategy :: Strategy
win_previousStrategy (latest:rest) = beat latest
win_previousStrategy []            = Rock

lose_previousStrategy :: Strategy
lose_previousStrategy (latest:rest) = lose latest
lose_previousStrategy []            = Paper

-- 8.4)
notLoseStrategy :: Move -> Move
notLoseStrategy m
    | beat pos1 == pos2 = pos2
    | otherwise         = pos1
    where
        possibleMoves = [x | x <- moves, x /= m]
        pos1          = head possibleMoves
        pos2          = last possibleMoves

last_twoStrategy :: Strategy
last_twoStrategy (fMove:sMove:rest)
    | fMove == sMove = notLoseStrategy fMove
    | otherwise      = randomStrategy []
last_twoStrategy _   = randomStrategy []

-- 8.5)
-- Frequency order: Rock, Paper, Scissors
movesFrequency :: [Move] -> [Int]
movesFrequency ms = [rockFreq, paperFreq, scissFreq]
    where
        rockFreq  = sum [1 | m <- ms, m == Rock]
        paperFreq = sum [1 | m <- ms, m == Paper]
        scissFreq = sum [1 | m <- ms, m == Scissors]

mostFrequent :: [Int] -> Move
mostFrequent fqs
    | maxFreq == fqs !! 0 = convertToMove 0
    | maxFreq == fqs !! 1 = convertToMove 1
    | otherwise           = convertToMove 2
    where
        maxFreq = maximum fqs

least_frequentStrategy :: Strategy
least_frequentStrategy ms = notLoseStrategy ((mostFrequent . movesFrequency) ms)

-- 8.6) Bullsnitz!

-- 8.7)
alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2
    | randInt 2 == 0 = str1
    | otherwise      = str2