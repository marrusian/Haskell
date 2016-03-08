-- 6.53)

data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Eq, Show)

type Value = Int

type Card = (Suit, Value)

type Deck = [Card]

-- 6.55)
type Name   = String
type Points = Int

type Player = (Name, Points)

-- 6.56)
type Trick = (Player, [(Player, Card)])

-- 6.57)
winNT :: Trick -> Player
winNT (lead, play) = [p | (p,c) <- play, snd c == maxVal] !! 0
    where
        maxVal = maximum [snd c | (p,c) <- play]
  --  
-- 6.58)
winT :: Suit -> Trick -> Player
winT trumpSuit (lead, play)
    | trumpPlayed = [p | (p,c) <- play, fst c == trumpSuit && snd c == maxTrumpVal] !! 0
    | otherwise   = winNT (lead, play)
    where
       trumpPlayed = [c | (p,c) <- play, fst c == trumpSuit] /= []
       maxTrumpVal = maximum [snd c | (p,c) <- play, fst c == trumpSuit]

-- 6.59)
type Hand = (Player, [Card])

-- 6.60)
type Hands = [Hand]

trickIns :: Trick
trickIns = (
              ("East",0), [
                           (("East", 0), (Hearts, 12)), (("South", 0), (Hearts,2)),
                           (("West", 0), (Spades, 6)),  (("North", 0), (Hearts, 14))
                          ]
           )

handsIns :: Hands
handsIns = [
            (("North",0), [(Spades, 11), (Spades, 2),  (Hearts, 14),  (Hearts, 7),  (Diamonds, 3)]),
            (("East", 0), [(Spades,  3), (Hearts, 12), (Diamonds, 7), (Diamonds, 2),(Clubs, 9)]),
            (("South",0), [(Spades, 14), (Spades, 13), (Spades, 12),  (Hearts, 2),  (Clubs, 11)]),
            (("West", 0), [(Spades,  6), (Diamonds,9), (Diamonds, 5), (Clubs, 14),  (Clubs, 8)])
           ]

-- 6.61)
checkPlay :: Hands -> Trick -> Bool
checkPlay hs (lead, play)
    | isPossible && isLegal = True
    | otherwise             = False
    where
        isPossible = and [c `elem` cs | (tp,c) <- play, (hp, cs) <- hs, hp == tp]
        isLegal    = and [null [sc | sc <- cs, fst sc == leadingSuit] | (tp,c) <- play, (hp, cs) <- hs, hp == tp, fst c /= leadingSuit]
            where
                leadingSuit = [fst c | (p, c) <- play, p == lead] !! 0