--2.1 

classify :: Int -> String 
classify n  
    | n <= 9 = "failed"
    | n >= 10 && n <= 12 = "passed"
    | n >= 13 && n <= 15 = "good"
    | n >= 16 && n <= 18 = "very good"
    | n >= 19 && n <= 20 = "excellent"
    | otherwise          = "Invalid Number!"

-- 2.2

classifyBMI :: Float -> Float -> String 
classifyBMI weight height 
    | bmi < 18.5 = "underweight"
    | bmi >= 18.5 && bmi < 25 = "normal weight"
    | bmi >= 25 && bmi < 30   = "overweight"
    | otherwise               = "obese"
    where bmi = weight / (height*height)


-- 2.3 a)
--max, min :: Ord a => a -> a -> a 
--max x y = if x >= y then x else y 
--min x y = if x <= y then x else y


max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = if x >= y && x >= z then x
             else if y >= x && y >= z then y
             else z 

min3 x y z = if x <= y && x <= z then x
             else if y <= x && y <= z then y
             else z 

--2.3 b)
newmax3, newmin3 :: Ord a => a -> a -> a -> a
newmax3 x y z = if max x y == x && max x z == x then x 
                else if max x y == y && max y z == y then y
                else z
newmin3 x y z = if min x y == x && min x z == x then x 
                else if min x y == y && min y z == y then y
                else z

-- 2.4 
exclusiveOr :: Bool -> Bool -> Bool 
exclusiveOr True False = True 
exclusiveOr True True = False 
exclusiveOr False False = True
exclusiveOr False True = False

-- 2.5 
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- 2.6 a) 

short :: [a] -> Bool
short list = if length list < 3 then True
             else False

-- 2.6 b)
myshort :: [a] -> Bool
myshort [] = True
myshort [_] = True
myshort [_,_] = True
myshort _ = False

-- 2.7 
median :: Ord a => a -> a -> a -> a 
median a b c 
    | a >= b && a <= c = a 
    | a >= c && a <= b = a 
    | b >= a && b <= c = b
    | b >= c && b <= a = b
    | otherwise = c 

mymedian :: (Num a, Ord a) =>  a -> a -> a -> a
mymedian a b c = a + b + c - (min3 a b c) - (max3 a b c)


-- 2.8 
propDivs :: Integer ->  [Integer]
propDivs n = [d | d <- [1..n `div` 2], n `mod` d == 0]

-- 2.9 
perfects :: Integer -> [Integer]
perfects n = [p | p <- [1..n], p == sum (propDivs p)]

--2.10 
pyths :: Integer -> [(Integer, Integer, Integer)]
pyths n = [(x,y,z) | x <- [1..n] ,y <- [1..n] ,z <- [1..n] , x*x + y*y == z*z]

--2.11
isPrime :: Integer -> Bool 
isPrime n = null [p | p <- [2..n-1], n `mod` p == 0]

-- 2.12
myconcat :: [[a]] -> [a]
myconcat listOfList = [x | xs <- listOfList, x <- xs]