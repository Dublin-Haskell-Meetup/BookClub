data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

myInfo = Book 21 "Snow Crash" ["Neill", "Stevenson"]

type CustomerID = Int

type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

data BookReview = BookReview BookInfo CustomerID String

type BookRecord = (BookInfo, BookReview)

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)
