-- file: ch03/BookStore.hs

data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
  ["Richard Bird","Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID String
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

data Customer = Customer {
	customerID :: CustomerID
   , customerName :: String
   , customerAddress :: Address
} deriving (Show)

customer1 = Customer 39448 "Bob Robertson"
  ["5 Fives Drive", "London, UK"]

customer2 = Customer {customerID = 39448
  , customerAddress = ["314 Pi Drive", "Crescent City, CA"]
  , customerName = "Bob del Norte"}

data List a = Cons a (List a)
  | Nil
  deriving (Show)