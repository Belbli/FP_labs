import Data.Maybe
import Data.List

amember :: Double -> Double -> Int -> Double
amember a1 dt n = a1 + dt * fromIntegral(n)

test1 = (amember 1 2 5, amember 1 10 12, amember 1 2 0)

f :: Double -> Double -> Double
f x 0 = x
f x n = f(x - ((x-cos(x))/(1+sin(x)))) (n - 1)

root :: Double -> Double
root 0 = 1
root n = f 1 n

test2 = (root 0, root 1, root 2, root 3, root 4, root 5)



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


test3 = (maxThree' [3, 8, 6, 5, 1], maxThree' [1, 2], maxThree'  [2], maxThree' [])


getStringBeforeRegex :: String -> Char -> String
getStringBeforeRegex [] _ = []
getStringBeforeRegex (x:xs) regex = if(x /= regex) then x : getStringBeforeRegex xs regex else getStringBeforeRegex [] regex

splitWithoutFisrt :: String -> Char -> [String]
splitWithoutFisrt [] _ = []
splitWithoutFisrt (x:xs) regex = if(x == regex) then getStringBeforeRegex (xs) regex : splitWithoutFisrt xs regex else splitWithoutFisrt xs regex

split :: String -> Char -> [String]
split [] _ = []
split str regex = getStringBeforeRegex str regex : splitWithoutFisrt str regex

test4 = (split "one,two,,three" ',', split "qwesa dsfr, regregfg. Qweefdfd, fdgdfv dfv dfvdfv dfvd, fvdfv drgertr." '.', split "" '_')

data BinaryTree a =
    Empty
    | Node (BinaryTree a) a (BinaryTree a)
    | Leaf a
    deriving (Show)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node leftSubTree a rightSubTree) = Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)

instance Functor BinaryTree where
    fmap = treeMap

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Leaf a) = [a]
preOrder (Node leftSubTree a rightSubTree) = [a] ++ preOrder leftSubTree ++ preOrder rightSubTree

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Leaf a) = [a]
inOrder (Node leftSubTree a rightSubTree) = inOrder leftSubTree ++ [a] ++ inOrder rightSubTree

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Leaf a) = [a]
postOrder (Node leftSubTree a rightSubTree) = postOrder leftSubTree ++ postOrder rightSubTree ++ [a]

maxDepth :: BinaryTree a -> Int
maxDepth Empty = 0
maxDepth (Leaf a) = 1
maxDepth (Node leftSubTree a rightSubTree) = 1 + max (maxDepth leftSubTree) (maxDepth rightSubTree)


{- 

                1
               / \
              3   2
            /  \
           6    5
         /  \
        7
       /
      10


bt = Node(Node (Node (Node (Leaf (10)) 7 Empty) 6 Empty) 3 (Leaf(5))) 1 (Leaf (2))


 -}