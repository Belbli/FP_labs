data Product = Book String String | VideoCassete String | CD String String Integer deriving (Show, Eq)
{-
    let c = Book "qwe" "asd"
    let c1 = Book "gfgf" "vdffg"
    let c2 = Book "ytge" "vfgsd"
    let c3 = CD "qwe" "vfrever" 12
    let list = c : c1 : c2 : c3 : []
-}
getTitle :: Product -> String
getTitle (Book n _) = n
getTitle (VideoCassete n) = n
getTitle (CD n _ _) = n

getTitles :: [Product] -> [String]
getTitles [] = []
getTitles (x:xs) = getTitle x : getTitles xs

getAuthor :: Product -> String
getAuthor (Book _ a) = a

bookAuthors :: [Product] -> [String]
bookAuthors [] = []
bookAuthors (x:xs) = getAuthor x : bookAuthors xs

lookUpTitle :: String -> [Product] -> Maybe Product
lookUpTitle "" _ = Nothing
lookUpTitle _ [] = Nothing
lookUpTitle s (x:xs) = if (s == getTitle x) then Just x else lookUpTitle s xs

lookUpTitles :: [String] -> [Product] -> [Maybe Product]
lookUpTitles [] [] = []
lookUpTitles [] (_:_) = []
lookUpTitles (x:xs) list = if(lookUpTitle x list /= Nothing) then lookUpTitle x list : lookUpTitles xs list else lookUpTitles xs list

data Suit = Club | Diamond | Heart | Spade deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven
              | Eight | Nine | Ten | Jack | Queen
              | King | Ace deriving (Show, Eq, Ord)

type Card = (Value, Suit)
type Deck = [Card]

isMinor :: Card -> Bool
isMinor (v, _) = if(v /= Jack && v /= Queen && v /= King && v /= Ace) then True else False

