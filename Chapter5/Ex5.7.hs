module Ex57 where
    
    import Test.QuickCheck
    import Test.HUnit

    type Person   = String
    type Book     = String
    type Database = [(Person, Book)]

    exampleBase :: Database
    exampleBase = [("Alice", "Tintin"), ("Anna", "Little Women"),
                   ("Alice", "Asterix"), ("Rory", "Tintin")]

    books       :: Database -> Person -> [Book]
    borrowers   :: Database -> Book -> [Person]
    borrowed    :: Database -> Book -> Bool
    numBorrowed :: Database -> Person -> Int
    makeLoan    :: Database -> Person -> Book -> Database
    returnLoan  :: Database -> Person -> Book -> Database

    books db prs         = [book | (person, book) <- db, person == prs]
    makeLoan db prs bk   = [(prs, bk)] ++ db
    returnLoan db prs bk = [loan | loan <- db, loan /= (prs, bk)]

    prop_db1 :: Database -> Person -> Book -> Bool
    prop_db1 db prs bk = elem bk loanedAfterLoan == True
        where
            afterLoan       = makeLoan db prs bk
            loanedAfterLoan = books afterLoan prs

    prop_db2 :: Database -> Person -> Book -> Bool
    prop_db2 db prs bk = elem bk loanedAfterReturn == False
        where
            afterReturn       = returnLoan db prs bk
            loanedAfterReturn = books afterReturn prs

    -- 5.28)
    borrowers db bk    = [person | (person, book) <- db, book == bk]
    borrowed db bk     = borrowers db bk /= []
    numBorrowed db prs = length (books db prs)

    -- 5.30)
    data Loan = Loan Person Book
                deriving (Eq)

    type Database' = [Loan]

    exampleBase' :: Database'
    exampleBase' = [Loan "Alice" "Tintin",  Loan "Anna" "Little Women",
                    Loan "Alice" "Asterix", Loan "Rory" "Tintin"]

    instance Show Loan where
        show (Loan prs bk) = "\nName: \t" ++ prs ++
                             "\nBook: \t" ++ bk ++ "\n"

    books'       :: Database' -> Person -> [Book]
    borrowers'   :: Database' -> Book -> [Person]
    borrowed'    :: Database' -> Book -> Bool
    numBorrowed' :: Database' -> Person -> Int
    makeLoan'    :: Database' -> Person -> Book -> Database'
    returnLoan'  :: Database' -> Person -> Book -> Database'

    books' db prs         = [book | (Loan person book) <- db, person == prs]
    borrowers' db bk      = [person | (Loan person book) <- db, book == bk]
    borrowed' db bk       = borrowers' db bk == []
    numBorrowed' db prs   = length (books' db prs)
    makeLoan' db prs bk   = [(Loan prs bk)] ++ db
    returnLoan' db prs bk = [loan | loan <- db, loan /= (Loan prs bk)]
