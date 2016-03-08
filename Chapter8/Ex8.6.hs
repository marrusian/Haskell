import RPS

convertMove :: Char -> Move
convertMove ch
    | ch == 'r' || ch == 'R' = Rock
    | ch == 'p' || ch == 'P' = Paper
    | otherwise              = Scissors

showResults :: Tournament -> IO ()
showResults t =
    do let res = tournamentOutcome' t
       putStrLn (case compare res 0 of
                   GT -> "I Won!"
                   EQ -> "Draw!"
                   LT -> "You won: well done!")

play :: Strategy -> IO ()
play strategy = playInteractive strategy ([], [])

playInteractive :: Strategy -> Tournament -> IO ()
playInteractive strat t@(mine, yours) =
    do ch <- getChar
       if not (ch `elem` "rpsRPS")
        then do putStr "\n\n"
                showResults t
        else do let next = strat yours
                putStrLn ("\nI Play: " ++ show next ++ " you play: " ++ [ch])
                let yourMove = convertMove ch
                playInteractive strat (next:mine, yourMove:yours)

step :: Strategy -> Strategy -> Tournament -> Tournament
step stratA stratB (msA, msB) = (stratA msB : msA, stratB msA : msB)

-- 8.20)
playSvsS :: Strategy -> Strategy -> Integer -> Tournament
playSvsS stratA stratB n
    | n > 0     = step stratA stratB (playSvsS stratA stratB (n-1))
    | otherwise = ([],[])

-- 8.21)
showTournament :: Tournament -> String
showTournament t@(msA, msB) = "PlayerA's score: "   ++ show playerAScore ++
                              "\nPlayerB's score: " ++ show playerBScore ++
                              "\nMoves: " ++ show t
    where
        tournScore = abs (tournamentOutcome' t)
        playerAScore = tournScore
        playerBScore = length msA - tournScore
