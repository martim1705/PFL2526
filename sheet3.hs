-- 3.1 a)
myand :: [Bool] -> Bool 
myand [] = True
myand (x:xs)
    | x == True = myand xs
    | x == False = False

-- 3.1 b) 
myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) 
    | x == False = myor xs
    | otherwise = True

-- 3.1 c) [[1,2,3],[4,5,6]] -> [1,2,3,4,5,6] 
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs


-- 3.1 d) 
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x

-- 3.1 e) 
myexclamation :: [a] -> Int -> a
myexclamation (x:_) 0 = x
myexclamation (x:xs) i = myexclamation xs (i-1)

-- 3.1 f) 

myelem :: Eq a => a -> [a] -> Bool
myelem t (x:_) | t == x = True
myelem t [] = False
myelem t (x:xs) = myelem t xs