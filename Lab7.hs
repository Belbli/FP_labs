import Data.Char

data Records = Remind String (Integer,Integer)  | --Напоминание (имя, дата (день, месяц))
               PhoneBook String String | --Запись о телефоне (имя, телефон)
               Meet (Integer,Integer,Integer) String --Встреча (дата (день, месяцб год), описание)
               deriving (Show,Eq)
               
getByName:: [Records] -> String -> (Maybe String, Maybe (Integer, Integer) )
getByName l p = (getB l, getR l)
    where 
        getB [] = Nothing
        getB (PhoneBook n p' :xs)
            | p' == p = Just n
            | otherwise = getB xs
        getB (_:xs) = getB xs
        getR [] = Nothing
        getR ( Remind p' d :xs)
            | p' == p = Just d
            | otherwise = getR xs
        getR (_:xs) = getR xs
        
unical n l
  | elem n l = l
  | otherwise = n:l
 
getByLetter:: [Records] -> Char -> [String]
getByLetter [] _ = []
getByLetter ( PhoneBook _ [] :xs) c = getByLetter xs c
getByLetter ( PhoneBook _ p@(n:_) :xs) c
    | n == c = unical p (getByLetter xs c)
    | otherwise = getByLetter xs c
getByLetter ( Remind [] _ :xs) c = getByLetter xs c
getByLetter ( Remind p@(n:_) _ :xs) c
    | n == c = unical p (getByLetter xs c)
    | otherwise = getByLetter xs c
getByLetter (_:xs) c = getByLetter xs c
 
getAssignment:: [Records] -> (Integer,Integer,Integer)  -> [Records] 
getAssignment [] _ = []
getAssignment (r@(Remind _ (d',m')):xs) (d,m,y)
    | d == d' && m == m' = r : getAssignment xs  (d,m,y)
    | otherwise = getAssignment xs (d,m,y)
getAssignment (r@(Meet (d',m',y') _):xs) (d,m,y)
    | d == d' && m == m' && y == y' = r : getAssignment xs (d,m,y)
    | otherwise = getAssignment xs (d,m,y)
getAssignment (_:xs) (d,m,y) = getAssignment xs (d,m,y)
 
testData = [
   Remind "Distaid" (18,4), 
   Remind "Jason" (4, 8),
   PhoneBook "8 (800) 200-23-16" "Distaid",
   PhoneBook "8 (495) 952-88-33" "Yandex",
   Meet (18,4,2020) "Delivery of the project"
    ]

main1 = do
    print $ getByName testData "Distaid"
    print $ getByLetter testData 'J'
    print $ getAssignment testData (18,4,2020)


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

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Не сдано"
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