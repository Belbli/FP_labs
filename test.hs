import Data.Maybe
import Data.List

amember :: Double -> Double -> Int -> Double
amember a1 dt n = a1 + dt * fromIntegral(n)

f :: Double -> Double -> Double
f x 0 = x
f x n = f(x - ((x-cos(x))/(1+sin(x)))) (n - 1)

root :: Double -> Double
root 0 = 0
root n = f 1 n

{- x = elemIndex (maximum [1, 4, 42, 43, 32, 54]) [1, 4, 42, 43, 32, 54] -}



maxThree :: [Int] -> [Int]
maxThree [] = []
maxThree (x:xs) = map (\ t -> if((fromJust(elemIndex t (x:xs)) - 1) == fromJust(elemIndex(maximum (x:xs)) (x:xs))
    ||(fromJust(elemIndex t (x:xs)) + 1) == fromJust(elemIndex(maximum (x:xs)) (x:xs))) then maximum (x:xs) else t) (x:xs)

maxThree' :: [Int] -> [Int]
maxThree' [] = []
maxThree' (x:xs) = map (\ t -> if(fromJust(elemIndex t (x:xs)) == (maxElIndex - 1)
    || fromJust(elemIndex t (x:xs)) == (maxElIndex + 1)) then maximum (x:xs) else t) (x:xs)
    where maxEl = maximum(x:xs)
          maxElIndex = fromJust(elemIndex (maxEl) (x:xs))


-- Тесты.
test = (maxThree [3, 8, 6, 5, 1], maxThree [1, 2], maxThree[2], maxThree [])

test2 = (maxThree' [3, 8, 6, 5, 1], maxThree' [1, 2], maxThree'  [2], maxThree' [])