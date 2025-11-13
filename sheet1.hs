incr :: Int -> Int 
incr x = x + 1

triple :: Int -> Int 
triple x = x * 3 

welcome :: String -> String 
welcome name = "Hello " ++ name ++ "!"

count :: String -> String 
count str = show (length str) ++ " characters."

-- exercicio 1.2 
-- leftHalf [1,2,3,4,5,6,7] == [1,2,3]
-- rightHalf [1,2,3,4,5,6,7] == [4,5,6,7]

leftHalf :: [a] -> [a]
leftHalf [] = []
leftHalf [a] = [a]
leftHalf list = take (length list `div` 2) list 

rightHalf :: [a] -> [a]
rightHalf [] = []
rightHalf [a] = [a] 
rightHalf list = drop (length list `div` 2) list


-- 1.3 
-- (a) Define a function to get the second element from a list. Example: second [1,2,3,4] == 2 Investigate what happens if the list has fewer than 2 elements. 

second :: [a] -> a
second [] = error "List has less than 2 elements"
second [a] = error "List has less than 2 elements"
second list = head (drop 1 list)
-- (b) The function last gets the last element of a list. Example: last [1,2,3,4] == 4 Show that this function can be defined as a composition of the above functions. Can you find two distinct definitions? 
mylast :: [a] -> a
mylast [] = error "List is empty"
mylast [a] = a 
mylast list = last (drop 1 list)
-- (c) Define the init function that removes the last element from a list using the above functions. Example: init [1,2,3,4] == [1,2,3] Can you find two distinct definitions? 
myinit :: [a] -> [a]
myinit [] = [] 
myinit list = take (length list - 1) list
-- (d) Define a middle function that gives que the middle element in a list. Example: middle [3,2,1,4,5] == 1 Investigates what happens if the list has an even or odd number of elements. 
mymiddle :: [a] -> a
mymiddle [a] = a
mymiddle list = mymiddle (drop 1 (reverse list))
-- (e) Define a function checkPalindrome that checks if a string is a palindrome, i.e. if it is equal to its reverse. The result should be a truth value (Bool). Examplos: checkPalindrome "abba" == True checkPalindrome "abra" == False
checkPalindrome :: String -> Bool 
checkPalindrome word = reverse word == word 


--1.4 
checkTriangle :: Float -> Float -> Float -> Bool 
checkTriangle a b c = a < b + c && b < a + c && c < a + b

-- 1.5 
triangleArea :: Float -> Float -> Float -> Float 
triangleArea a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2