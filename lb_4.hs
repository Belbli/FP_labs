makePositive :: [Integer] -> [Integer]
makePositive [] = []
makePositive (x:xs) = abs(x) : makePositive xs

getN :: Int -> [Integer] -> Maybe Integer
getN _ [] = Nothing
getN n list = Just(list!!n)

countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue (x:xs) = if(x) then 1 + countTrue xs else countTrue xs

removeEmpty :: [String] -> [String]
removeEmpty n = filter(not . null) n

delete :: Char -> String -> String
delete ch str = filter(/=ch) str

deleteOdd :: [Integer] -> [Integer]
deleteOdd [] = []
deleteOdd (x:xs) = if (odd x) then deleteOdd xs else x : deleteOdd xs

substitute :: Char -> Char -> String -> String
substitute _ _ [] = []
substitute ch1 ch2 (x:xs) = if(x /= ch1) then x : substitute ch1 ch2 xs else ch2 : substitute ch1 ch2 xs 

doubleAvg :: [Double] -> Double
doubleAvg list = foldr (+) 0 list / foldr (\x y -> 1+y) 0 list

list1 :: Integer -> [Integer]
list1 0 = []
list1 x = list1 (x-1)++(x:[])

list2 :: Integer -> [Integer]
list2 0 = []
list2 x = list2 (x-1)++(2*x-1:[])

list3 :: Integer -> [Integer]
list3 0 = []
list3 x = list3 (x-1)++(2*x:[])

list4 :: Integer -> [Integer]
list4 0 = []
list4 x = list4 (x-1)++(x*x:[])

list5 :: Integer -> [Integer]
factorial 0 = 1
factorial x = x*factorial(x-1)
list5 0 = []
list5 x = list5 (x-1)++[factorial(x)]  

list6 :: Integer -> [Integer]
stepen 1 = 1
stepen x = 2*stepen(x-1)
list6 0 = []
list6 x = list6 (x-1)++[stepen(x+1)]

list7 :: Integer -> [Integer]
piram 1 = 1
piram(x) = x+piram(x-1)
list7 0 = []
list7 x = list7 (x-1)++[piram(x)]

list8 :: Integer -> [Integer]
pir 1 = 1
pir(x) = x+pir(x-1)
p 1 = 1
p x = pir(x) + p(x-1)
list8 0 = []
list8 x = list8 (x-1)++[p(x)]

listsSum :: [Integer] -> [Integer] -> [Integer]
listsSum [] [] = []
listsSum [] (x:xs) = x : listsSum [] xs
listsSum (x:xs) [] = x : listsSum [] xs
listsSum (x:xs) (y:ys) = x+y : listsSum xs ys

oddEvenSwap :: [Integer] -> [Integer]
oddEvenSwap [] = []
oddEvenSwap (x:[]) = x:[]
oddEvenSwap (x:y:[]) = if((odd x && even (y)) || (even (x) && odd (y))) then y : x : [] else x : y : []
oddEvenSwap (x:y:xs) = if((odd x && even (y)) || (even (x) && odd (y))) then y : x : oddEvenSwap xs else x : y : oddEvenSwap xs

pow2 :: Integer -> Integer
pow2 n | (n == 1) = 2
       | (even n) = w * w
       | otherwise = (w * w) + (w * w)
         where w = pow2 (n `div` 2)