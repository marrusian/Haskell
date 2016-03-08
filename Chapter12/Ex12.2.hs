module Ex122 where
    import System.Random
    import System.IO.Unsafe

    data Move = Rock | Paper | Scissors
                deriving (Eq, Show)

    beat :: Move -> Move
    beat Rock  = Paper
    beat Paper = Scissors
    beat _     = Rock

    type Strategy = [Move] -> Move

    alternate :: Strategy -> Strategy -> Strategy
    alternate str1 str2 moves =
         case length moves `rem` 2 of
            1 -> str1 moves
            0 -> str2 moves

    alternate' :: Strategy -> Strategy -> Strategy
    alternate' str1 str2 =
        \moves ->
            case length moves `rem` 2 of
                1 -> str1 moves
                0 -> str2 moves

    alternate'' :: Strategy -> Strategy -> Strategy
    alternate'' str1 str2 moves = map ($ moves) [str1,str2] !! (length moves `rem` 2)

    -- 12.7)
    sToss :: Strategy -> Strategy -> Strategy
    sToss str1 str2 = [str1, str2] !! randChoice
        where randChoice = unsafePerformIO $ getStdRandom (randomR (0,1))

    -- 12.8)
    alternativeList :: [Strategy] -> Strategy
    alternativeList [] _       = Rock
    alternativeList strs moves = (strs !! (length moves `rem` length strs)) moves

    -- 12.9)
    sTossList :: [Strategy] -> Strategy
    sTossList [] _  = randMove
        where randMove = [Rock, Paper, Scissors] !! (unsafePerformIO $ getStdRandom (randomR (0, 2)))

    sTossList strs moves = (strs !! randChoice) moves
        where
            randChoice = unsafePerformIO $ getStdRandom (randomR (0, length strs))
------------------------------------------------------------------------------
    beatStrategy :: Strategy -> Strategy
    beatStrategy opponent moves = beat (opponent moves)

    -- 12.11)
    getOccurences :: [Move] -> (Int, Int, Int) -> (Int, Int, Int)
    getOccurences (m:ms) (r,p,s) =
        case m of
            Rock     -> getOccurences ms (r+1,p,s)
            Paper    -> getOccurences ms (r,p+1,s)
            _        -> getOccurences ms (r,p,s+1)
    getOccurences _ _ = (0,0,0)

    majority :: [Strategy] -> Strategy
    majority strs moves 
        | rocks /= papers && papers /= scissors && scissors /= rocks =
            case mostChosen of
                rocks  -> Rock
                papers -> Paper
                _      -> Scissors
        | otherwise = randMove
        where
            options                   = map ($ moves) strs
            (rocks, papers, scissors) = getOccurences options (0,0,0)
            mostChosen                = max (max rocks papers) scissors
            randMove                  = [Rock, Paper, Scissors] !! (unsafePerformIO $ getStdRandom (randomR (0, 2)))

    -- 12.12) ???