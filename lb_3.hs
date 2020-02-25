module Lb_3 where

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = if a > b && a > c then a else if b > a && b > c then b else c

min3 :: Integer -> Integer -> Integer -> Integer
min3 a b c = if a < b && a < c then a else if b < a && b < c then b else c

bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _ _ = False

isSorted :: Integer -> Integer -> Integer -> Bool
isSorted a b c = if a < b && b < c then True else False

isTriangle :: Integer->Integer->Integer->Bool
isTriangle a b c = if a < (b+c) && b < (a+c) && c < (a+b) then True else False

swap :: [Integer] -> [Integer]
swap [] = []
swap [x] = [x]
swap (x:y:xs) = y:x:(swap xs)

s2 :: Integer -> Integer -> (Integer, Integer)
s2 x y = (max x y, min x y)

sort2 :: [Integer] -> [Integer]
sort2 (x:xs) = if (x:xs)!!0 > (x:xs)!!1 then swap (x:xs) else (x:xs)

solve2 :: Double -> Double -> (Bool, Double)
solve2 a b = if not (a == 0) then (True, -b/a) else (False, 0.0)

cornerCof :: (Double, Double) -> (Double, Double) -> Double
cornerCof (x1, y1) (x2, y2) = (y2-y1)/(x2-x1)

lineLength :: (Double, Double) -> (Double, Double) -> Double
lineLength (x1, y1) (x2, y2) = (x2-x1)^2 + (y2-y1)^2

isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1, y1) (x2, y2) (x3, y3) (x4, y4) = cornerCof(x1, y1) (x2, y2) == cornerCof(x3, y3) (x4, y4)

isRectengular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectengular (x1, y1) (x2, y2) (x3, y3) | lineLength (x2, y2) (x3, y3) == lineLength (x1, y1) (x2, y2) + lineLength(x1, y1) (x3, y3) ||  lineLength (x1, y1) (x2, y2) == lineLength (x2, y2) (x3, y3) + lineLength(x1, y1) (x3, y3) || lineLength(x1, y1) (x3, y3) == lineLength (x2, y2) (x3, y3) + lineLength (x1, y1) (x2, y2) = True
	 				 | otherwise = False

isIncluded :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
isIncluded (x1, y1, r1) (x2, y2, r2) = if sqrt(lineLength (x1, y1) (x2, y2)) == 0 && not (r1 == 0) && not (r2 == 0) || max r1 r2 - sqrt(lineLength (x1, y1) (x2, y2)) <= min r1 r2 && not (r1 == 0) && not (r2 == 0) then True else False


--isRectengular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
--isRectengular (x1, y1) (x2, y2) (x3, y3) = if cornerCof(x1, y1) (x2, y2) * cornerCof(x1, y1) (x3, y3) == -1 || cornerCof(x1, y1) (x2, y2) * cornerCof(x2, y2) (x3, y3) == -1 then True else False


