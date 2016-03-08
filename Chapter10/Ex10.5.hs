import Prelude hiding (lookup)
import qualified Pictures
import qualified Ex57
import qualified Ex67
import qualified Ex81

-- 10.29)
invertColour :: Pictures.Picture -> Pictures.Picture
invertColour = map $ map invert
    where invert :: Char -> Char
          invert ch = if ch == '.' then '#' else '.'

-- 10.30)
superimpose :: Pictures.Picture -> Pictures.Picture -> Pictures.Picture
superimpose = zipWith $ zipWith combine
    where combine :: Char -> Char -> Char
          combine topCh bottomCh
            | topCh == '.' && bottomCh == '.' = '.'
            | otherwise                       = '#'

-- 10.31) ???

------------------------------------------------------------------------
-- 10.32)
books       :: Ex57.Database -> Ex57.Person -> [Ex57.Book]
books db prs = map snd $ filter personMatch db
    where
        personMatch :: (Ex57.Person, Ex57.Book) -> Bool
        personMatch (p,b) = prs == p 

borrowers   :: Ex57.Database -> Ex57.Book -> [Ex57.Person]
borrowers db bk = map fst $ filter bookMatch db
    where
        bookMatch :: (Ex57.Person, Ex57.Book) -> Bool
        bookMatch (p,b) = bk == b

borrowed    :: Ex57.Database -> Ex57.Book -> Bool
borrowed db bk = not . null $ borrowers db bk

numBorrowed :: Ex57.Database -> Ex57.Person -> Int
numBorrowed db prs = length $ books db prs

makeLoan    :: Ex57.Database -> Ex57.Person -> Ex57.Book -> Ex57.Database
makeLoan db prs bk = [(prs, bk)] ++ db

returnLoan  :: Ex57.Database -> Ex57.Person -> Ex57.Book -> Ex57.Database
returnLoan db prs bk = filter loanMatch db
    where
        loanMatch :: (Ex57.Person, Ex57.Book) -> Bool
        loanMatch loan = loan /= (prs,bk)

------------------------------------------------------------------------

-- 10.33)
type Database' = [(Ex57.Person, [Ex57.Book])]

exampleBase' :: Database'
exampleBase' = [("Alice", ["Tintin", "Asterix"]), ("Anna", ["Little Women"]),
                ("Rory", ["Tintin"])]

books'       :: Database' -> Ex57.Person -> [Ex57.Book]
books' ((p,bs):db) prs
    | p == prs  = bs
    | otherwise = books' db prs
books' _ _ = []

bookMatch :: Ex57.Book -> Ex57.Book -> Bool
bookMatch bk1 bk2 = (bk1 == bk2)

borrowers'   :: Database' -> Ex57.Book -> [Ex57.Person]
borrowers' db searchedBook = map fst $ filter hasBook db
    where
        hasBook :: (Ex57.Person, [Ex57.Book]) -> Bool
        hasBook (_, bks) = not . null $ dropWhile (not . bookMatch searchedBook) bks

borrowed'   :: Database' -> Ex57.Book -> Bool
borrowed' db bk = not . null $ borrowers' db bk

numBorrowed' :: Database' -> Ex57.Person -> Int
numBorrowed' db prs = length $ books' db prs

makeLoan'    :: Database' -> Ex57.Person -> [Ex57.Book] -> Database'
makeLoan' db prs bks = [(prs, bks)] ++ db

returnBook'  :: [Ex57.Book] -> Ex57.Book -> [Ex57.Book]
returnBook' bs bk = filter (not . bookMatch bk) bs

returnBooks' :: [Ex57.Book] -> [Ex57.Book] -> [Ex57.Book]
returnBooks' bs (bk:bks) = returnBooks' (returnBook' bs bk) bks
returnBooks' bs  _ = bs

cleanDatabase' :: Database' -> Database'
cleanDatabase' db = filter emptyBooks db
    where
        emptyBooks :: (Ex57.Person, [Ex57.Book]) -> Bool
        emptyBooks (p, bks) = not . null $ bks

returnLoan' :: Database' -> Ex57.Person -> [Ex57.Book] -> Database'
returnLoan' db prs bks
    = cleanDatabase' [(p, if p==prs then returnBooks' bs bks else bs) | (p, bs) <- db]

------------------------------------------------------------------------

-- 10.34)

-- 6.41)
formatLines :: Ex67.BillType -> String
formatLines = concat . map Ex67.formatLine

-- 6.42)
makeTotal :: Ex67.BillType -> Ex67.Price
makeTotal = sum . map snd

-- 6.46)
lookup :: Ex67.BarCode -> (Ex67.Name, Ex67.Price)
lookup = Ex67.look Ex67.codeIndex

-- 6.47)
makeBill :: Ex67.TillType -> Ex67.BillType
makeBill = filter (("Unknown Item", 0) /= ) . map lookup

-- 6.48)
makeDiscount :: Ex67.BillType -> Ex67.Price
makeDiscount pairs = (sum . map (const 100) . filter isDrySherry $ pairs) `div` 2
    where
        isDrySherry (n, p) = n == "Dry Sherry, 1lt"

------------------------------------------------------------------------

-- 10.36)
tournamentOutcome :: (Integral a) => Ex81.Tournament -> a
tournamentOutcome = sum . map (uncurry Ex81.outcome) . uncurry zip
