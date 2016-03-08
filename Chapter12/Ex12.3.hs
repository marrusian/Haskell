module Ex123 where
    import Prelude hiding ((<*>))
    import Data.Char

    type RegExp = String -> Bool

    epsilon :: RegExp
    epsilon = (== "")

    char :: Char -> RegExp
    char ch = (== [ch])

    (|||) :: RegExp -> RegExp -> RegExp
    (|||) e1 e2 = (\x -> e1 x || e2 x)

    (<*>) :: RegExp -> RegExp -> RegExp
    (<*>) e1 e2 = (\x -> or [e1 y && e2 z | (y,z) <- splits x])

    (<**>) :: RegExp -> RegExp -> RegExp
    (<**>) e1 e2 = (\x -> or [e1 y && e2 z | (y,z) <- tail $ splits x])
    
    star :: RegExp -> RegExp
    star p = epsilon ||| (p <**> star p)

    -- 12.13)
    splits :: String -> [(String, String)]
    splits str = [splitAt i str | i <- [0..length str]]

    -- 12.14)
    a, b :: RegExp
    (a,b) = (char 'a', char 'b')

    -- 12.15)
    -- star ((a ||| b) <*> (a ||| b)) == {"", "aa", "ab", "ba", "bb", "aaaa", "aaab"
    --                                     "aaba", "aabb", "abaa", "abab", "abba",
    --                                     "abbb", "baaa", "baab", "baba", "babb",
    --                                     "bbaa", "bbab", "bbba", "bbbb", ....}

    -- 12.16)
    option :: RegExp -> RegExp
    option e = epsilon ||| e

    plus :: RegExp -> RegExp
    plus e = e ||| (e <**> plus e)

    -- 12.17)
    digit :: RegExp
    digit = char '0' ||| char '1' ||| char '2' ||| char '3' |||
            char '4' ||| char '5' ||| char '6' ||| char '7' |||
            char '8' ||| char '9'

    nzdigit :: RegExp
    nzdigit r = r /= "0" && digit r

    digits :: RegExp
    digits = nzdigit <*> star digit

    fractional :: RegExp
    fractional = nzdigit <*> star digit <*> char '.' <*> star digit <*> nzdigit

    -- 12.18)
    rex1 :: RegExp
    rex1 = star b <*> option a <*> star b <*> option a <*> star b

    rex2 :: RegExp
    rex2 = star b <*> a <*> star b <*> a <*> star b
