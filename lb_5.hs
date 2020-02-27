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

{-
    let c1 = (Two, Club)
    let c2 = (Ace, Club)
    let c3 = (Two, Spade)
    let c4 = (Ace, Spade)
    let list = c1 : c2 : c3 : []
-}

isMinor :: Card -> Bool
isMinor (v, _) = if(v /= Jack && v /= Queen && v /= King && v /= Ace) then True else False

sameSuit :: Card -> Card -> Bool
sameSuit (_, s1) (_, s2) = s1 == s2

beats :: Card -> Card -> Bool
beats (v1, s1) (v2, s2) = if(sameSuit (v1, s1) (v1, s2) && v1 > v2) then True else False

beats2 :: Suit -> Card -> Card -> Bool
beats2 trump (v1, s1) (v2, s2) = if(sameSuit (v1, s1) (v1, s2) && v1 > v2) then True else if (s1 == trump) then True else False

beatsList :: [Card] -> Card -> Suit -> [Card]
beatsList [] _ _ = []
beatsList (x:xs) card trump = if(beats2 trump card x) then x : beatsList xs card trump else beatsList xs card trump

cardValue :: Card -> Integer
cardValue (v, _) = case v of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11

getValue :: Card -> Value
getValue (v, _) = v

valueWithoutAce :: [Card] -> Integer
valueWithoutAce [] = 0
valueWithoutAce (x:xs) = if(getValue x /= Ace) then cardValue x + valueWithoutAce xs else valueWithoutAce xs

countAces :: [Card] -> Integer
countAces [] = 0
countAces (x:xs) = if(getValue x == Ace) then 1 + countAces xs else countAces xs

addT :: Integer -> [Integer] -> [Integer]
addT 0 x = x
addT n x = addT (n-1) $ (map (+1) x) ++ (map (+11) x)
-- 5 15 -> 6 16 16 26
getUniq :: [Integer] -> [Integer] -> [Integer]
getUniq [] r = r
getUniq (x:xs) r | (x `elem` r) = getUniq xs r
                 | otherwise = getUniq xs (x:r) 

task :: [Card] -> [Integer]
task c = getUniq (addT nt [swt]) []
         where nt  = countAces c
               swt = valueWithoutAce c