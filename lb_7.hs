import Data.Maybe
data Date = DM {day :: Integer, month :: Integer} | DMY {day :: Integer, month :: Integer, year :: Integer} deriving(Show, Eq)
{- data Birthday = Birthday {name :: String, date :: Date}
data Phone = Phone {contactName :: String, phone :: Integer}
data Meeting = Meeting {dateOfMeeting :: Date, about :: String} -}

data Note = Birthday {name :: String, date :: Date}
             | Phone {contactName :: String, phone :: String}
             | Meeting {dateOfMeeting :: Date, about :: String} deriving(Show, Eq)

type Elbook = [Note]
{-
    let b = Birthday "Andrew" (DM 7 5)
    let b1 = Phone "Andrew" "+375-33-336-27-48"
    let b2 = Meeting (DMY 7 5 2020) "Andrew celebrates birthday"
    let b3 = Phone "Ann" "+375-33-333-33-33"
    let b4 = Birthday "Borya" (DM 10 12)
    let b5 = Meeting (DMY 10 11 2020) "Important deal"
    let b6 = Meeting (DMY 7 5 2020) "Smth important"
    let elBook = b : b1 : b2 : b3 : b4 : b5 : b6 : []
    getBirthdaysAndPhones elBook (DMY 7 5 2020)
-}

getName :: Note -> Maybe String
getName (Birthday n _) = Just n
getName (Phone n _) = Just n
getName _ = Nothing

getDate :: Note -> Maybe Date
getDate (Birthday _ date) = Just date
getDate (Meeting date _) = Just date
getDate _ = Nothing

getByName :: Elbook -> String -> Elbook
getByName [] _ = []
getByName (x:xs) name = if(getName x /= Nothing && fromJust(getName x) == name) then x : getByName xs name else getByName xs name

getByLetter :: Elbook -> Char -> Elbook
getByLetter [] _ = []
getByLetter(x:xs) ch = if(getName x /= Nothing && fromJust(getName x)!!0 == ch) then x : getByLetter xs ch else getByLetter xs ch

getDM :: Date -> Date
getDM (DMY d m _) = DM d m
getDM (DM d m) = DM d m

getMeetingDate :: Note -> Maybe Date
getMeetingDate (Meeting date _) = Just date
getMeetingDate _ = Nothing

getDealsByDate :: Elbook -> Date -> Elbook
getDealsByDate [] _ = []
getDealsByDate (x:xs) date = if(getDate x /= Nothing && fromJust(getDate x) == date) then x : getDealsByDate xs date
    else getDealsByDate xs date

isPhone :: Note -> Bool
isPhone (Phone _ _) = True
isPhone _ = False

getPhone :: Elbook -> String -> Maybe Note
getPhone [] _ = Nothing
getPhone (x:xs) s = if(isPhone x && fromJust(getName x) == s) then Just x else getPhone xs s


getBirthdaysAndPhones :: Elbook -> Date -> Elbook
getBirthdaysAndPhones [] _ = []
getBirthdaysAndPhones (x:xs) (DMY d m y) = if(getDate x /= Nothing && fromJust(getDate x) == (DM d m)) then fromJust(getPhone (x:xs) (fromJust(getName x))) : getBirthdaysAndPhones xs (DMY d m y)
    else getBirthdaysAndPhones xs (DMY d m y)

getByAssigment :: Elbook -> Date -> Elbook
getByAssigment [] _ = []
getByAssigment elb date = getDealsByDate elb date ++ getBirthdaysAndPhones elb date

{- g :: Elbook -> Date -> Elbook
g [] _ = []
fromJust(getPhone (x:xs) (fromJust(getName x)))
g elb date = getDealsByDate elb date ++ getDealsByDate elb date -}