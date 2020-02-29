import Data.Maybe
import Data.Char
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
    getByAssigment elBook (DMY 7 5 2020)
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

data KeyCode = Sym Char | CapsLock | Shift Char deriving (Eq,Show)
 
getSym :: KeyCode -> Bool -> Char
getSym (Sym a) False = a
getSym (Sym a) True = toUpper a
getSym (Shift '1') _ = '!'
getSym (Shift '2') _ = '@'
getSym (Shift '3') _ = '#'
getSym (Shift '4') _ = '$'
getSym (Shift '5') _ = '%'
getSym (Shift '6') _ = '^'
getSym (Shift '7') _ = '&'
getSym (Shift '8') _ = '*'
getSym (Shift '9') _ = '('
getSym (Shift '0') _ = ')'
getSym (Shift '-') _ = '_'
getSym (Shift '=') _ = '+'
getSym (Shift a) cl | cl == False = if (toUpper a == a) then toLower a else toUpper a
                    | cl == True = if (toUpper a == a) then toUpper a else toLower a
 
getString :: [KeyCode] -> Bool -> String
getString [] _ = ""
getString (CapsLock:ks) cl = getString ks (not cl)
getString (k:ks) cl = (getSym k cl) : getString ks cl
                        
getAlNum :: [KeyCode] -> [KeyCode]
getAlNum [] = []    
getAlNum (CapsLock:ks) = getAlNum ks
getAlNum (k:ks)            = k : getAlNum ks
 
getRaw :: [KeyCode] -> String
getRaw [] = ""
getRaw (CapsLock:ks) = getRaw ks
getRaw (Sym k:ks) = k : getRaw ks
getRaw (Shift k:ks)  = k : getRaw ks
 
isCapsLocked  :: [KeyCode] -> Bool -> Bool
isCapsLocked [] cl  = cl
isCapsLocked (CapsLock:ks) cl = isCapsLocked ks (not cl)
isCapsLocked (k:ks) cl = isCapsLocked ks cl



data Task = Laba {subject :: String, labaNum :: Integer}
            |Rgz {rgzSubject :: String}
            |Referat {refSubject :: String, theme :: String} deriving(Show, Eq)

data Lesson = Lesson {task :: Task, completionWeek :: Maybe Integer} deriving(Show, Eq)
type Plan = [Lesson]

{-
    let t = [Laba "FP" 7, Rgz "KSIS", Referat "FP" "User types in Haskell", Laba "FP" 6, Rgz "Math", Rgz "Art"]
    let c = [Lesson (Laba "FP" 7) (Just 4), Lesson (Rgz "KSIS") (Just 5), Lesson (Referat "FP" "User types in Haskell") Nothing, Lesson (Laba "FP" 6) Nothing]
-}

getTitle :: Lesson -> String
getTitle (Lesson (Laba s _) _) = s
getTitle (Lesson(Rgz s) _) = s
getTitle (Lesson(Referat s _) _) = s

getByTitle :: Plan -> String -> Plan
getByTitle [] _ = []
getByTitle (x:xs) title = if(getTitle x == title) then x : getByTitle xs title else getByTitle xs title

getReferates :: Plan -> [Task]
getReferates [] = []
getReferates ((Lesson(Referat s t)n):xs) = (Referat s t) : getReferates xs
getReferates (x:xs) = getReferates xs

isDone :: Lesson -> Maybe Integer
isDone (Lesson(Laba _ _)n) = n
isDone (Lesson(Rgz _)n) = n
isDone (Lesson(Referat _ _)n) = n

getRest :: Plan -> Plan
getRest [] = []
getRest (x:xs) = if(isDone x == Nothing) then x : getRest xs else getRest xs
