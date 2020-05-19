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

{-      22222222222222222222
        22222222222222222222
        2222            2222
        2222            2222
        2222            2222
                        2222
                        2222
                        2222
        22222222222222222222
        22222222222222222222
        2222    
        2222
        2222
        2222
        2222
        22222222222222222222
        22222222222222222222
 -}     

data Keys = Shift Char | CapsLock | Symb Char deriving (Show,Eq)

getAlNum :: [Keys] -> [Keys]
getAlNum [] = []
getAlNum (n@(Symb s) :t) = n : getAlNum t
getAlNum (_ :t) = getAlNum t

getRaw :: [Keys] -> String
getRaw [] = ""
getRaw (Symb s :t) = s : getRaw t
getRaw (_ :t) = getRaw t

isCapsLocked :: [Keys] -> Bool -> Bool
isCapsLocked [] b = b
isCapsLocked (h:t) b = if (h == CapsLock) then isCapsLocked t (not b) else isCapsLocked t b

getString :: [Keys] -> Bool -> String
getString [] b = []
getString (CapsLock :t) b = getString t (not b)
getString (Symb c :t) b = if b then (toUpper c) : (getString t b) else c : (getString t b)
getString (Shift c :t) b = if b then (toLower c) : (getString t b) else (toUpper c) : (getString t b)

test = [Symb 'a',CapsLock,Symb 's',Shift 'u',Symb 'e',CapsLock,Symb 'z']

main2 = do
    print $ getAlNum test
    print $ getRaw test
    print $ isCapsLocked test False
    print $ getString test False



data Task = Lab String Integer     --Название предмета, номер лабораторной работы
            | RGZ String           --Название предмета
            | Report String String --Название предмета, тема реферата
            deriving (Show, Eq)

getSubj :: Task -> String
getSubj (Lab s n) = s
getSubj (RGZ s) = s
getSubj (Report s n) = s
getByTitle :: [(Task, Maybe Integer)] -> String ->  [Task]
getByTitle list s = map (\ (x,y) -> x) (filter (\ (x,y) -> ((getSubj x) == s) && (y == Nothing)) list)

isReferat :: (Task, Maybe Integer) -> Bool
isReferat ((Report _ _), _ ) = True
isReferat _ = False
getReferats :: [(Task, Maybe Integer)]  -> [Task]
getReferats list = map (\ (x,y) -> x) (filter isReferat list)

getRest :: [(Task, Maybe Integer)] -> [Task]
getRest list = map (\ (x,y) -> x) (filter (\ (x,y) -> (y == Nothing)) list)
{- 
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Не сдано" -}
getRestForWeek :: [(Task, Maybe Integer)] -> Integer -> [Task]
getRestForWeek list z = map (\ (x,y) -> x) (filter (\ (x,y) -> (y == Nothing) || (fromJust y > z)) list)

numWeek :: [(Task, Maybe Integer)] -> Integer -> Integer
numWeek list z = foldl (+) 0 (map (\ (x,y) -> if y == Just z then 1 else 0) list)
uniList :: [Integer] -> [Integer]
uniList [] = []
uniList (x:xs) | x `elem` xs = uniList xs
               | otherwise = x : uniList xs
listWeek :: [(Task, Maybe Integer)] -> [Integer]
listWeek list = uniList (map (\ (x,y) -> fromJust y) (filter (\ (x,y) -> (y /= Nothing)) list))
getPlot :: [(Task, Maybe Integer)] -> [(Integer,Integer)]
getPlot list = map (\ x -> (x, numWeek list x)) (listWeek list)

plan = [((Lab "OSiSP" 1 ),(Just 1)),
    ((Lab "OSiSP" 2 ),(Nothing)),
    ((Report "OSiSP" "Threads" ),(Nothing)),
    ((RGZ "LabView2" ),(Just 2)),
    ((RGZ "LabView4" ),(Nothing)),
    ((Lab "FP" 1 ),(Just 3)),
    ((Lab "FP" 2 ),(Just 5)),
    ((Lab "FP" 3 ),(Just 7)),
    ((Report "OSiSP" "Linux" ),(Nothing))]

main3 = do
    print $ getByTitle plan "OSiSP"
    print $ getReferats plan
    print $ getRest plan
    print $ getRestForWeek plan 3
    print $ getPlot plan



{- 
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
getRest (x:xs) = if(isDone x == Nothing) then x : getRest xs else getRest xs -}



