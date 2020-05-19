import Data.Maybe
data Font = Courier | Lucida | Fixedsys deriving (Show, Eq)

{-
    let list = [Circle 1 2 3, Rectangle 1 2 4 5, Circle 2 2 5, Rectangle 5 6 1 0]
-}

data Figure = Circle {cx :: Double, cy :: Double, r :: Double}
             | Rectangle {x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double}
             | Tryangle {x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double, x3 :: Double, y3 :: Double}
             | TextBox {bx :: Double, by :: Double, fontFamily :: Font, text :: String} deriving (Show, Eq)

area :: Figure -> Double
area (Circle _ _ r) = 3.14*r*r
area (Rectangle x1 y1 x2 y2) = abs(x1-x2)*abs(y1-y2)
area (Tryangle x1 y1 x2 y2 x3 y3) = 1/2 * abs((x1-x3) * (y2-y3) - (x2-x3) * (y1-y3))
area (TextBox _ _ f s) = (fromIntegral (length s)) * (getLetterSize f)


isRect :: Figure -> Bool
isRect (Rectangle _ _ _ _) = True
isRect _ = False

getLetterSize :: Font -> Double
getLetterSize Courier  = 8
getLetterSize Fixedsys = 10
getLetterSize Lucida   = 12

getRects :: [Figure] -> [Figure]
getRects [] = []
getRects (x:xs) = if(isRect x) then x : getRects xs else getRects xs

getBound :: Figure -> Figure
getBound (Rectangle x1 y1 x2 y2) = (Rectangle x1 y1 x2 y2)
getBound (Circle x y r) = (Rectangle (x-r) (y+r) (x+r) (y-r))
getBound (Tryangle x1 y1 x2 y2 x3 y3) = (Rectangle x1 y2 x3 y3)
getBound (TextBox x y f s) = (Rectangle x (y+(getLetterSize f)) (x+(fromIntegral (length s)) * (getLetterSize f)) y)

isInBound :: Figure -> (Double, Double) -> Bool
isInBound (Rectangle x1 y1 x2 y2) (x, y) = x > x1 && x < x2 && y < y1 && y > y2

getBounds :: [Figure] -> [Figure]
getBounds [] = []
getBounds (x:xs) = getBound x : getBounds xs

getFigure :: [Figure] -> (Double, Double) -> Maybe Figure
getFigure [] _ = Nothing
getFigure (x:xs) (cx, cy) = if(isInBound(getBound x) (cx, cy)) then Just x else getFigure xs (cx, cy)

move :: Figure -> Double -> Double -> Figure
move (Rectangle x1 y1 x2 y2) dx dy      = (Rectangle (x1+dx) (y1+dy) (x2+dx) (y2+dy))
move (Circle x y r) dx dy               = (Circle (x+dx) (y+dy) r)
move (TextBox x y f s) dx dy            = (TextBox (x+dx) (y+dy) f s)
move (Tryangle x1 y1 x2 y2 x3 y3) dx dy = (Tryangle (x1+dx) (y1+dy) (x2+dx) (y2+dy) (x3+dx) (y3+dy))


data Estate = Apartment {floor :: Integer, square :: Double, flours :: Integer}
            | Room {floor :: Integer, square :: Double, roomSquare :: Double, flours :: Integer}
            | House {square :: Double} deriving(Show, Eq)

data Object = Object {objType :: Estate, price :: Double} deriving(Show, Eq)

data Query = Query {minSquare :: Double, maxPrice :: Double, minFloor :: Integer, maxFloor :: Integer} deriving(Show, Eq)

--type Apartment = {floor :: Integer, square :: Double, flours :: Integer}

{-
    let ap = [Apartment]
    let a1 = Apartment 10 56.6 20
    let a2 = Room 5 56.6 16.8 20
    let a3 = House 120.4
    let a4 = Apartment 3 72.4 10
    let a5 = Room 1 56.6 16.6 20
    let list = [Apartment 10 56.6 20, Room 5 56.6 16.8 20, House 120.4, Apartment 3 72.4 10]
    let objs = [Object a1 45300, Object a2 400, Object a3 80500, Object a4 54600, Object a5 350]
    let req = Requirements 60 100000 2 10
-}

isHouse :: Estate -> Bool
isHouse (House _) = True
isHouse _ = False

getHouses :: [Estate] -> [Estate]
getHouses [] = []
getHouses (x:xs) = if(isHouse x) then x : getHouses xs else getHouses xs

getPrice :: Object -> Double
getPrice (Object _ p) = p

getByPrice :: [Object] -> Double -> [Object]
getByPrice [] _ = []
getByPrice (x:xs) price = if(price == getPrice x) then x : getByPrice xs price else getByPrice xs price

getFloor :: Object -> Maybe Integer
getFloor (Object (Apartment x _ _) _) = Just x
getFloor (Object (Room x _ _ _) _) = Just x
getFloor _ = Nothing

getFloorBound :: Object -> Maybe Integer
getFloorBound (Object (Apartment _ _ x) _) = Just x
getFloorBound (Object (Room _ _ _ x) _) = Just x
getFloorBound _ = Nothing

getByLvl :: [Object] -> Integer -> [Object]
getByLvl [] _ = []
getByLvl (x:xs) floor = if(getFloor x /= Nothing && fromJust(getFloor x) == floor) then x : getByLvl xs floor else getByLvl xs floor

getExceptBounds :: [Object] -> [Object]
getExceptBounds [] = []
getExceptBounds (x:xs) = if(getFloor x /= Nothing && fromJust(getFloor x) > 1
    && getFloor x < getFloorBound x) then x : getExceptBounds xs else getExceptBounds xs

getSq :: Object -> Double
getSq (Object (Apartment _ s _) _) = s
getSq (Object (Room _ _ s _) _) = s
getSq (Object (House s)_) = s

query :: [Object] -> Query -> [Object]
query [] _ = []
query (x:xs) (Query minSquare maxPrice minFloor maxFloor) = if(getFloor x /= Nothing && fromJust(getFloor x) >= minFloor
     && fromJust(getFloor x) <= maxFloor && getSq x >= minSquare) then x : query xs (Query minSquare maxPrice minFloor maxFloor)
     else query xs (Query minSquare maxPrice minFloor maxFloor)
