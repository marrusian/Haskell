module Ex67 where
  
  import Prelude hiding (lookup)

  type Name    = String
  type Price   = Int
  type BarCode = Int

  type Database = [(BarCode, Name, Price)]

  codeIndex :: Database
  codeIndex = [
                (4719, "Fish Fingers",       121),
                (5643, "Nappies",           1010),
                (3814, "Orange Jelly",        56),
                (1111, "Hula Hoops",          21),
                (1112, "Hula Hoops (Giant)", 133),
                (1234, "Dry Sherry, 1lt",    540)
              ]

  type TillType = [BarCode]
  type BillType = [(Name, Price)]

  lineLength :: Int
  lineLength = 30

  produceBill :: TillType -> String
  produceBill = formatBill . makeBill

  -- 6.39)
  formatPence :: Price -> String
  formatPence p = show quotient ++ (if remainder<10 then ".0" else ".") ++ show remainder
      where
          quotient  = p `div` 100
          remainder = p `mod` 100

  -- 6.40)
  formatLine :: (Name, Price) -> String
  formatLine (n, p) = n ++ (replicate numDots '.') ++ formatedPrice ++ "\n"
      where
          formatedPrice = formatPence p
          numDots = lineLength - (length n + length formatedPrice)

  -- 6.41)
  formatLines :: BillType -> String
  formatLines pairs = concat [formatLine (n, p) | (n, p) <- pairs]

  -- 6.42)
  makeTotal :: BillType -> Price
  makeTotal pairs = sum [p | (n, p) <- pairs]

  -- 6.43)
  formatTotal :: Price -> String
  formatTotal p = "\nTotal" ++ (replicate numDots '.') ++ formatedPrice
      where
          formatedPrice = formatPence p
          numDots = lineLength - (length "Total" + length formatedPrice)

  -- 6.44)
  formatBill :: BillType -> String
  formatBill pairs = "\t Haskell Stores \t\n\n" ++
                     formatLines pairs          ++
                     (if discount /= 0 then formatDiscount discount else "") ++
                     formatTotal (makeTotal pairs - discount)
      where
          discount = makeDiscount pairs

  -- 6.45)
  look :: Database -> BarCode -> (Name, Price)
  look dBase bCode
      | billList /= [] = head billList
      | otherwise      = ("Unknown Item", 0)
      where
          billList = [(n,p) | (c,n,p) <- dBase, c == bCode]

  -- 6.46)
  lookup :: BarCode -> (Name, Price)
  lookup bCode = look codeIndex bCode

  -- 6.47)
  makeBill :: TillType -> BillType
  makeBill bars = [billItem | bCode <- bars, let billItem = lookup bCode, fst billItem /= "Unknown Item" && snd billItem /= 0]

  -- 6.48)
  makeDiscount :: BillType -> Price
  makeDiscount pairs = sum [100 | (n,p) <- pairs, n == "Dry Sherry, 1lt"] `div` 2

  formatDiscount :: Price -> String
  formatDiscount p = "\nDiscount" ++ (replicate numDots '.') ++ formatedPrice ++ "\n"
      where
          formatedPrice = formatPence p
          numDots = lineLength - (length "Discount" + length formatedPrice)

  -- 6.49)
  removeItem :: Database -> BarCode -> Database
  removeItem dBase bCode = [(b,n,p) | (b,n,p) <- dBase, b /= bCode]

  addItem :: Database -> BarCode -> (Name, Price) -> Database
  addItem dBase bCode (newName, newPrice) = [(bCode, newName, newPrice)] ++ removeItem dBase bCode

  -- 6.50)
  -- Reffer to makeBill()
