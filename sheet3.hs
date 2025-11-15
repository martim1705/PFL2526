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


-- 3.2 a) 
leastDiv :: Integer -> Integer 
leastDiv x = leastDivF 2 x

leastDivF :: Integer -> Integer -> Integer 
leastDivF d n 
    | d * d > n = n -- sqrt(n) * sqrt(n) == n 
    | n `mod` d == 0 = d  -- n / d = k 
    | otherwise = leastDivF (d+1) n

-- 3.2 b) 
-- isPrimeFast :: Integer -> Bool
--isPrimeFast n  
--    | n <= 1 = False
--    | leastDiv n == n = True
--    | otherwise = False
--
--
-- 3.3

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)


-- 3.4 

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse n (x:xs) = x : n : intersperse n xs


-- 3.5 a) 
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) 
    | n <= x = n : x : xs
    | otherwise = x : insert n xs

-- 3.5 b) 
isort :: Ord a => [a] -> [a] 
isort [] = [] 
isort (x:xs) = insert x ( isort xs) 

-- 3.6 a) 

merge :: Ord a => [a] -> [a] -> [a] 
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
    else y : merge (x:xs) ys

-- 3.6 b)
msort :: Ord a => [a] -> [a] 
msort [] = []
msort [a] = [a]
msort list = merge (msort left) (msort right)
    where (left, right) = splitAt (length list `div` 2) list


-- 3.7 

toBits :: Int -> [Int]
toBits n = reverse (arraybits n)

arraybits :: Int -> [Int]
arraybits 0 = [0]
arraybits 1 = [1] 
arraybits n = n `mod` 2 : arraybits (n `div` 2)


-- 3.8 
fromBits :: [Int] -> Int
fromBits [1] = 1 
fromBits [0] = 0
fromBits (x:xs) = x * (2 ^ (length (xs))) + fromBits xs


-- 3.9 

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0 ) [1..n]

-- 3.10 
isPrimeFast :: Integer -> Bool
isPrimeFast n 
    | n > 1 && all (\p -> n `mod` p /= 0) [2..floor (sqrt (fromIntegral n))] = True
    | otherwise = False

-- 3.12 

fromBitsI :: [Int] -> Int 
fromBitsI list = foldl (\acc bit -> acc * 2 + bit) 0 list 

-- 3.13 

group :: Eq a => [a] -> [[a]]
group [] = [] 
group (x:xs) = 
    let first = x : takeWhile (== x) xs
        rest = dropWhile (== x) xs 
    in first : group rest

-- 3.14 a) 
intercalate :: a -> [a] -> [[a]]
intercalate n [] = [[n]]
intercalate n (x:xs) = (n:x:xs) : map (x:) (intercalate n xs) 

-- 3.14 b) 
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = 
    concatMap (intercalate x) (permutations xs)