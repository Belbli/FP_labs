--t2 = take 2 (head(tail [[1,2],[3,4,5]]))!!1

--t3 = snd(take 2 (tail [(1,'a'),(2,'b'),(3,'c'),(4,'d')])!!1)

t1 :: [Integer]->Integer
t1 (x:xs) = head(tail(tail(tail (take 4 (x:xs)))))
t1 [] = error "emty list"

t2 :: [[Integer]]->Integer
t2 (x:xs) = take 2 (head(tail (x:xs)))!!1
t2 [] = error "emty list"

t3 :: [(Integer, Char)]->Char
t3 (x:xs) = snd(take 2 (tail (x:xs))!!1)
t3 [] = error "emty list"

t4 :: [(Integer,Char)]->[Char]
t4 (x:xs) = snd x : (snd(head xs)) : []
