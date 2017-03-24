-- hutton.hs --

import Data.Char

-- Tests --

hutton = 

  -- 7.4 foldl function --

  -- 7.3 foldr function --


  -- 7.2 Processing lists --
  remdupsFl [1, 2, 2, 3, 3, 3, 1, 1]

  --zip [0..30] (fibs 30)

  --all even [2,4,6,8]
  
  --any odd [2,4,6,8]
  
  --takeWhile even [2,4,5,8,9] -- take elements from a list as long as they satisfy the predicate; stop taking elements from a list as soon as we encounter an element which doesn't satisfy the predicate.
  
  --dropWhile odd [1,3,2,4,5,8,9] -- drop elements from a list as long as they satisfy the predicate; stop taking elements from a list as soon as we encounter an element which doesn't satisfy the predicate.
  
  --mapHut (mapHut (+1)) [[38,27],[43,3,9,82,10]]
  --mapHut (mapHut (mapHut (+1))) [[[1,2],[3,4]],[[5,6]], [[7]]]

  -- 5.3 zip function --
  --zip ['a', 'b', 'c'] [1,2,3,4,5]
  
  -- Basic concepts --
  --concat2 [['a','b','c'], ['w','e','r']]
  
  -- Exercises 4.8 --
  
  -- halve [1,2,3,4,5,6]

  -- Operator Sections --
  -- (1+) 2
  -- (+1) 4
  -- (*2) 4
  -- (1/) 100
  -- (/2) 100

  -- testPatternList ['a', 'b', 'c']
  -- testPatternListWithCons ['b', 'b', 'c']
  
  -- add2 doOnce (mul doTwice doTwice) (+1) 0
  -- mul doTwice doThreeTimes (+1) 0
  -- add2 doOnce doTwice (+1) 0
  -- ("decr 9 = "); (decr 9)
  -- isZero getZero
  -- getOne
  -- getZero





-- Script --

-- Functions --

{-
Find K-complementary pairs in a given array of integers. 
Given an integer K, and an array A, the pair (i, j) is said to be 
K-complementary if A[i] + A[j] = K.
-}
pairsKComplementary :: [Int] -> Int -> [(Int,Int)]
pairsKComplementary [] _ = []
pairsKComplementary (n:ns) k = (pairsKComplementary' n ns k) ++ (pairsKComplementary ns k)

pairsKComplementary2 :: [Int] -> Int -> [(Int,Int)]
pairsKComplementary2 (n:ns) k = [(i,j) | i <- (n:ns), j <- ns, i+j == k] 

pairsKComplementary' :: Int -> [Int] -> Int -> [(Int, Int)]
pairsKComplementary' _ [] k = [] 
pairsKComplementary' n ns k = [(n,m) | m <- ns, n+m == k]

pairsKComplementary'' :: Int -> [Int] -> Int -> [(Int, Int)]
pairsKComplementary'' _ [] k = [] 
pairsKComplementary'' i (n:ns) k | i+n == k  = (i,n) : pairsKComplementary'' i ns k
                                 | otherwise = pairsKComplementary'' i ns k

-- Chapter 7: Higher-order functions --


-- 7.4 The foldl function --

sumHut3 :: Num a => [a] -> a
sumHut3 = sumHut3' 0
  where
    sumHut3' v []     = v
    sumHut3' v (x:xs) = sumHut3' (v + x) xs

-- f v []     = v
-- f v (x:xs) = f (v # x) xs

-- foldl (#) v [x0, x1, ... , xn] = (... ((v# x0) # x1) ...) # xn



foldlHut :: (a -> b -> a) -> a -> [b] -> a
foldlHut f v []     = v
foldlHut f v (x:xs) = foldlHut f (f v x) xs


sumFl :: Num a => [a] -> a
sumFl = foldl (+) 0

prodFl :: Num a => [a] -> a
prodFl = foldl (*) 1

orFl :: [Bool] -> Bool
orFl = foldl (||) False

andFl :: [Bool] -> Bool
andFl = foldl (&&) True

lengthFl :: [a] -> Int
lengthFl = foldl inc 0
  where
    inc :: Int -> a -> Int
    inc n x     = n + 1

reverseFl :: [a] -> [a]
reverseFl = foldl rev [] 
  where
    rev :: [a] -> a -> [a]
    rev xs x = [x] ++ xs 
 
remdupsFl :: Eq a => [a] -> [a]
remdupsFl = foldl appendOrNot []

appendOrNot :: Eq a => [a] -> a -> [a]
appendOrNot xs y =
  if (has xs y)
    then xs
    else xs ++ [y]

has :: Eq a => [a] -> a -> Bool
has [] _ = False
has (x:xs) y | y == x = True 
             | y /= x = has xs y


-----------------------------
-- f v []     = v
-- f v (x:xs) = f (v # x) xs

-- Think of the behavior of foldl in a non-recursive manner, in terms of
-- an operator (#) that is assumed to associate to the left, as summarised by the following equation.
-- foldl (#) v [x0, x1, ... , xn] = (... ((v # x0) # x1) ...) # xn

facFl :: Int -> Int -- Takes an Integer and returns its fac.
facFl n = foldl (*) 1 [1..n]

concatFl :: [[a]] -> [a]
concatFl = foldl (++) []

appendListFl :: [a] -> [a] -> [a] -- Takes 2 lists and appends the 2nd to the 1st.
appendListFl xs ys = foldl snoc' xs ys

snoc' :: [a] -> a -> [a] -- Reverse of cons. Takes a list and an element and appends the element to the list.
snoc' xs x = xs ++ [x]

sumOfSquaresFl, sumOfSquaresFl2 :: Integral a => a -> a -- Take a number and return the sum of the squares of all the numbers from zero upto that number.

sumOfSquaresFl n = foldl (+) 0 [x^2 | x <- [1..n]]

sumOfSquaresFl2 n = foldl (\y x -> x^2 + y) 0 [1..n]

removeSubstringFl, removeSubstringFl' :: String -> String -> String -- Takes two strings and returns the first string without the chars in the second string. 

removeSubstringFl xs ys = foldl removeCharL xs ys

removeSubstringFl' = foldl removeCharL

removeCharL :: String -> Char -> String -- Takes a string and a char, and returns a new string without the char. 
removeCharL [] x = []
removeCharL (x:xs) y | x == y = removeCharL xs y
                     | x /= y = x : removeCharL xs y

keepSubstring {-, keepSubstringFl -} :: String -> String -> String -- Takes two strings and returns the second string with the chars of the first string only, and none other chars.

keepSubstring [] [] = []
keepSubstring [] ys = []
keepSubstring xs [] = []
keepSubstring xs (y:ys) | has xs y  = y : keepSubstring xs ys
                        | otherwise = keepSubstring xs ys

--keepSubstringFl xs ys = foldl keepCharL' xs ys  

keepCharL {-, keepCharL' -} :: String -> Char -> String -- Takes a char and a string and returns a new string with the char only, and none others. 

keepCharL [] _ = []
keepCharL (x:xs) y | x == y    = x : keepCharL xs y
                   | otherwise = keepCharL xs y

{-
keepCharL' [] _ = []
keepCharL' (x:xs) y | has (x:xs) y = x : keepCharL' xs y
                    | otherwise    = keepCharL' xs y
-}



--------------------------------



















-- 7.3 The foldr function --

-- f []     = v
-- f (x:xs) = x # f xs


foldrHut :: (a -> b -> b) -> b -> [a] -> b
foldrHut f v []     = v
foldrHut f v (x:xs) = f x (foldrHut f v xs)

sumHutFr :: Num a => [a] -> a
sumHutFr = foldr (+) 0 -- Replace each cons by '+' and [] by 0.

sumHutFrArg :: Num a => [a] -> a
sumHutFrArg xs = foldr (+) 0 xs

productHutFr :: Num a => [a] -> a
productHutFr = foldr (*) 1 -- Replace each cons by '*' and [] by 1.

orHutFr :: [Bool] -> Bool
orHutFr = foldr (||) False -- Replace each cons by '||' and [] by False.

andHutFr :: [Bool] -> Bool
andHutFr = foldr (&&) True -- Replace each cons by '&&' and [] by True.

lengthLC :: [a] -> Int
lengthLC [] = 0
lengthLC xs = sum [1 | x <- xs]

lengthR :: [a] -> Int
lengthR [] = 0
lengthR (_:xs) = 1 + lengthR xs 

lengthFr :: [a] -> Int
lengthFr = foldr (\_ n -> 1 + n) 0 

lengthFr2 :: [a] -> Int
lengthFr2 = foldr (\x -> \n -> 1 + n) 0 

--lengthFr3 :: [a] -> Int
--lengthFr3 = foldr (\n -> 1 + n) 0 

reverse2, reverseL, reverseL2, reverse3, reverseFr :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

reverseL []     = []
reverseL (x:xs) = (\x -> \y -> x ++ y) (reverseL xs) [x]

reverseL2 []     = []
reverseL2 (x:xs) = (\x y -> x ++ y) (reverseL2 xs) [x]

reverse3 []     = []
reverse3 (x:xs) = snoc x (reverse3 xs)

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverseFr = foldr snoc []

sumHutFr2 :: Num a => [a] -> a
sumHutFr2 (x:xs) = (+) x (foldr (+) 0 xs)

-----------------------------

-- Think of the behavior of foldr in a non-recursive manner, in terms of an operator (#) that 
-- is assumed to associate to the right, as summarised by the following equation.
-- foldr (#) v [x0, x1, ... , xn] = x0 # (x1 # ... (xn # v) ...)

facFr :: Int -> Int
facFr n = foldr (*) 1 [1..n] -- Takes an Integer and returns its fac.

concatFr :: [[a]] -> [a]
concatFr = foldr (++) []

appendListFr :: [a] -> [a] -> [a]
appendListFr xs ys = foldr (:) ys xs -- Takes 2 lists and appends the 2nd to the 1st.

sumOfSquares2, sumOfSquares3, sumOfSquaresFr, sumOfSquaresFr2 :: Integral a => a -> a -- Take a number and return the sum of the squares of all the numbers from zero upto that number.

sumOfSquares2 0 = 0
sumOfSquares2 n = sum [x^2 | x <- [1..n]]

sumOfSquares3 0 = 0
sumOfSquares3 n = (+) (n^2) (sumOfSquares3 ((-) n 1))

sumOfSquaresFr n = foldr (+) 0 [x^2 | x <- [1..n]]

sumOfSquaresFr2 n = foldr (\x y -> x^2 + y) 0 [1..n]

removeSubstringFr :: String -> String -> String -- Takes two strings and returns the first string without the second string. 
removeSubstringFr xs ys = foldr removeCharR xs ys

removeCharR :: Char -> String -> String -- Takes a char and a string and returns a new string without the char. 
removeCharR x [] = []
removeCharR x (y:ys) | x == y = removeCharR x ys
                    | x /= y = y : removeCharR x ys

--keepSubstring :: String -> String -> String -- Takes two strings and returns the second string with the first string only, and none other chars.
--keepSubstring xs ys = foldr keepChar xs ys

keepChar :: Char -> String -> String -- Takes a char and a string and returns a new string with the char only, and none others. 
keepChar x [] = []
keepChar x (y:ys) | x == y = y : keepChar x ys
                  | x /= y = keepChar x ys




--------------------------------











-- 7.2 Processing lists --
mapHut :: (a -> b) -> [a] -> [b]
mapHut f xs = [f x | x <- xs] -- Defining map using a list comprehension: simpler.

mapHutRec :: (a -> b) -> [a] -> [b]
mapHutRec f []     = [] -- Defining map using recursion: preferable for reasoning purposes.
mapHutRec f (x:xs) = f x : map f xs

filterHut :: (a -> Bool) -> [a] -> [a]
filterHut p xs = [x | x <- xs, p x == True] -- Defining the function using a list comprehension: simpler. 

filterHutRec :: (a -> Bool) -> [a] -> [a]
filterHutRec p []     = [] -- Defining the function using recursion: preferable for reasoning purposes.
filterHutRec p (x:xs) | p x == True = x : filterHutRec p xs
                      | otherwise   = filterHutRec p xs

-- Return the sum of the squares of the even integers from a list.
sumOfSquaresOfEvens :: [Int] -> Int
sumOfSquaresOfEvens [] = 0
sumOfSquaresOfEvens ns = sum (mapHut (^2) (filterHut even ns))

sumOfSquaresOfEvensListComp :: [Int] -> Int
sumOfSquaresOfEvensListComp ns = sum [n | n <- mapHut (^2) (filterHut even ns)]


--------------------------


remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups [x] = [x]
remdups (x0:x1:xs) | x0 == x1  = remdups (x1:xs)
                   | otherwise = x0 : remdups (x1:xs)























-------------------------------

















-- Chapter 6: Recursive functions --

-- 6.8 Exercises --

-- 1. How does the recursive version of the fac function behave if applied to a negative
-- argument such as (-1)? Modify the definition to prohibit negative arguments by adding a guard to
-- the recursive case.
facNegNo :: Int -> Int
facNegNo 0 = 1
facNegNo n | n > 0 = n * facNegNo (n-1)

-- 2. Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative
-- integers from a given value down to zero. 
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n 
  | n < 0 = 0
  | n > 0 = n + sumdown (n-1)

-- 3. Define the exponentiation operator ^ for non-negative integers using the same pattern of
-- recursion as the multiplication operator *, and show how the expression 2 ^ 3 is evaluated 
-- using your definition.
mulHut2 :: Int -> Int -> Int
m `mulHut2` n | n <= 0 = 0
              | n > 0  = m + (m `mulHut2` (n-1))

expoHut :: Int -> Int -> Int
m `expoHut` n | n <= 0 = 1
              | n > 0  = m * (m `expoHut` (n-1))

-- 4. Define a recursive function euclid :: Int -> Int -> Int that implements Euclid's algorithm for
-- calculating the GCD of two non-negative integers: if the two numbers are equal, then this number
-- is the result; otherwise, the smaller number is subtracted from the larger, and the same process
-- is then repeated (with the smaller number and the difference).
euclid :: Int -> Int -> Int
euclid m n  | m <= 0, n <= 0  = 0
            | m == n          = m
            | m < n           = euclid m (n - m)
            | m > n           = euclid (m - n) n

-- Using the recursive functions defined in this chapter, show how length [1,2,3], drop 3 [1,2,3,4,5] and 
-- init 1,2,3]

-- 6. Without looking at the definitions from the standard prelude, define the following library
-- functions on lists using recursion.
--
-- a. Decide if all logical values in a list are True:
andHut :: [Bool] -> Bool
andHut []     = True -- Simple (base) case. Identity for && is True.
andHut (x:xs) = x && andHut xs 

-- b. Concatenate a list of lists
concatHut :: [[a]] -> [a]
concatHut []     = []
concatHut (x:xs) = x ++ concatHut xs
--concatHut (x:xs) = x : concatHut xs

-- c. Produce a list with n identical elements:
replicateHut :: Int -> a -> [a]
replicateHut 0 _ = []
replicateHut n x = x : replicateHut ((-) n 1) x 

-- d. Select the nth element of a list:
selectNth :: [a] -> Int -> a
selectNth (x:xs) 0 = x 
selectNth (x:xs) n | n > 0, n <= (length (x:xs)) = selectNth xs (n-1)

-- e. Decide if a value is an element of a list:
elemHut :: Eq a => a -> [a] -> Bool
elemHut _ []     = False
elemHut x (y:ys) | x == y = True 
                 | x /= y = False || elemHut x ys


-- 7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
-- lists to give a single sorted list.
-- Note: your definition should not use other functions on sorted lists, but should be defined
-- using explicit recursion.
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8. Using merge, define a function sortMerge :: Ord a => [a] -> [a] that implements merge sort,
-- in which the empty list and singleton lists are already sorted, and any other list is sorted by
-- merging together the two lists that result from sorting the two halves of the list separately.
-- Hint: first define the function halve :: [a] -> ([a],[a]) that splits a list into two halves
-- whose lengths differ by at most one.
sortMerge :: Ord a => [a] -> [a]
sortMerge [] = [] 
sortMerge [x] = [x]
sortMerge xs = merge (sortMerge (fst (halve2 xs))) (sortMerge (snd (halve2 xs))) 

halve2 :: [a] -> ([a],[a])
halve2 xs = splitAt (div (length xs) 2) xs


-- 9. Using the five-step process, construct the library functions that:
-- a. calculate the sum of a list of numbers;
-- b. take a given number of elements from the start of a list;
-- c. select the last element of a non-empty list.

sumHut :: Num a => [a] -> a
sumHut []     = 0
sumHut (x:xs) = (+) x (sumHut xs) 

takeHut :: Int -> [a] -> [a]
takeHut 0 xs = []
takeHut _ [] = []
takeHut n (x:xs) | n < 0  = []
                 | n >= 0 = x : takeHut (n-1) xs 

lastHut :: [a] -> a
lastHut [x] = x
lastHut (x:xs) = lastHut xs


-- 6.6 Advice on recursion --
-- Step 1: define the type
-- Step 2: enumerate the cases
-- Step 3: define the simple cases
-- Step 4: define the other cases
-- Step 5: generalise and simplify

-- Example 1: product
product3 :: Num a => [a] -> a
product3 []     = 1
product3 (n:ns) = n * product3 ns 
--product3 ns = head ns * product3 (tail ns)
--product3  = foldr (*) 1 -- Generalise and simplify

-- Example 2: drop
dropHut2 :: Int -> [a] -> [a] -- Step 1: 1) Int for simplicity, 2) Currying for flexibility, 3) Int before [a] for readability, 4) Polymorphic function in the type of the list elements for generality.
dropHut2 0 xs     = xs
dropHut2 _ []     = []
dropHut2 n (_:xs) = dropHut2 (n-1) xs

-- Example 3: init
initHut :: [a] -> [a]
initHut (x:xs) | null xs = []
               | otherwise = x : initHut xs

initHutSimplified :: [a] -> [a]
initHutSimplified [_]    = []
initHutSimplified (x:xs) = x : initHut xs


-- 6.5 Mutual recursion --

evenHut :: Int -> Bool
evenHut 0 = True
evenHut n = oddHut ((-) n 1)

oddHut :: Int -> Bool
oddHut 0 = False
oddHut n = evenHut ((-) n 1)

-- Counting from zero.
evens2 :: [a] -> [a]
evens2 [] = []
evens2 (x:xs) = x : odds2 xs -- x corresponds to the 0th position.

-- Counting from zero.
odds2 :: [a] -> [a]
odds2 [] = []
odds2 (_:xs) = evens2 xs -- _ is omitted since odds begin from the 1st position onwards.



-- 6.4 Multiple recursion --

fibHut :: Int -> Integer
fibHut 0 = 0
fibHut 1 = 1
fibHut n = (+) (fibHut ((-) n 1)) (fibHut ((-) n 2))

--fibHutSeq :: Int -> [Int]
--fibHutSeq n = [x | x <- fibHut n]

sortQuick :: Ord a => [a] -> [a]
sortQuick [] = []
sortQuick (x:xs) = sortQuick smaller ++ [x] ++ sortQuick larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]


-- 6.3 Multiple arguments -- 

zipHut :: [a] -> [b] -> [(a,b)]
zipHut [] _  = []
zipHut _ []  = []
zipHut (x:xs) (y:ys) = (x,y) : zipHut xs ys 

dropHut :: Int -> [a] -> [a]
dropHut 0 xs  = xs
dropHut _ [] = []
dropHut n (_:xs) = dropHut ((-) n 1) xs



-- 6.2 Recursion on lists --

productHut :: Num a => [a] -> a
productHut [] = 1
productHut (n:ns) = n * productHut ns

lengthHut :: [a] -> Int
lengthHut [] = 0
lengthHut (_:xs) = 1 + lengthHut xs

lengthHut2 :: [a] -> Int
lengthHut2 [] = 0
lengthHut2 (_:xs) = succ (lengthHut2 xs)

reverseHut :: [a] -> [a]
reverseHut [] = []
reverseHut (x:xs) = reverseHut xs ++ [x]

plusplusHut :: [a] -> [a] -> [a]
plusplusHut [] ys = ys
plusplusHut (x:xs) ys = x : (plusplusHut xs ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
  | x <= y    = x : y : ys
  | otherwise = y: insert x ys

sortInsertion :: Ord a => [a] -> [a]
sortInsertion []     = []
sortInsertion (x:xs) = insert x (sortInsertion xs)

append :: [a] -> [a] -> [a]
append xs [] = xs
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- 6.1 Basic concepts --
fac2 :: Int -> Int
fac2 0 = 1
fac2 n = n * fac2 (n-1)

mulHut :: Int -> Int -> Int
m `mulHut` 0 = 0
m `mulHut` n = m + (m `mulHut` (n-1))





-- Chapter 5: List Comprehensions --

-- 5.7 Exercises --

-- 1. Using a list comprehension, give an expression that calculates the sum
-- 1^2 + 2^2 _ ... + 100^2 of the first 100 integer squares.
sumOfSquares :: Integer
sumOfSquares = sum [x^2 | x <- [1..100]]

-- 2. Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)]
-- that returns a coordinate grid of a given size.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3. Using a list comprehension and the function grid above, 
-- define a function square :: Int -> [(Int, Int)] that returns a coordinate
-- square of size n, excluding the diagonal from (0,0) to (n,n).
squareNoDiagonal :: Int -> [(Int, Int)]
squareNoDiagonal n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 4. In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements can
-- be defined using a list comprehension.
lengthEx :: [a] -> Int
lengthEx xs = sum [1 | _ <- xs]

replicateEx :: Int -> a -> [a]
replicateEx n x = [ x | _ <- [1..n] ]

-- 5. Using a list comprehension with 3 generators, define a function that returns the list of
-- all such triples whose components are at most a given limit.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6. A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
-- Using a list comprehension and the functions factors, define a function that 
-- returns the list of all perfect numbers up to a given limit.
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factorsEx x)) == x]
  where
    factorsEx n = [x | x <- [1..n], mod n x == 0]

-- 7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with 2 generators can be
-- re-expressed using two comprehensions with single generators.
sol7 :: [(Integer, Integer)]
sol7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]] -- "2 dimensions"

sol7_2 :: [(Char, Char, Integer)]
sol7_2 = concat[ concat[ [ (x,y,z) | z <- [1,2] ] | y <- ['a','b']] | x <- ['@','#']] -- "3 dimensions"

-- 8. Redefine the function positions using the function find.

-- Return a list of all values mapped to a given key, 
-- from a list of key-value pairs.
findEx :: Eq a => a -> [(a,b)] -> [b]
findEx k t = [v | (k', v) <- t, k' == k]

-- Return a list of all positions at which a value occurs in a list, by 
-- pairing each element with its position, and selecting those positions at which
-- the desired value occurs.
positionsOfValInListEx :: Eq a => a -> [a] -> [Int]
positionsOfValInListEx x xs = findEx x (zip xs [0..])

-- 9. The scalar product of two lists is the sum of the products of the corresponding integers.
-- In a similar manner to chisqr, show how a list comprehension can be used to define a function
-- scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists.

chisqrEx :: [Float] -> [Float] -> Float
chisqrEx os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

scalarproduct xs ys = sum [ (x * y) | (x,y) <- zip xs ys]


-- 10. Modify the Caesar cipher program to also handle upper-case letters.

-- Return the integer between 0 and 57 corresponding to the letter and special character.
-- The special characters (located at positions 26 to 31) are: '[', '\', ']', '^', '_', '`' 
let2intEx :: Char -> Int
let2intEx c = ord c - ord 'A'

-- Return the character (including upper-case letters, and one of six special characters) 
-- corresponding to an integer between 0 and 57.
int2letEx :: Int -> Char
int2letEx n = chr (ord 'A' + n)

-- Return the character (letters and one of 6 specials) got after shifting a character,
-- by converting the character into the corresponding integer, adding on the shift factor and
-- taking the remainder when divided by 58, and converting the resulting
-- integer back into a character (letters and one of 6 specials).
-- Do not shift any other characters.
shiftEx :: Int -> Char -> Char
shiftEx n c
  | isLower c || isUpper c || c == '[' || c == '\\' || c == ']' || c == '^' || c == '_' || c == '`' 
    = int2letEx (mod (let2intEx c + n) 58)
  | otherwise = c

-- Return a string (containing both letters, lower- and upper-case, and the 6 special characters) which is encoded by
-- using the shift function and a shift factor.
encodeCaesarEx :: Int -> String -> String
encodeCaesarEx n xs = [shiftEx n x | x <- xs]




-- 5.5 The Caesar cipher --

-- Return the integer between 0 and 25 corresponding to the letter.
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- Return the character (letter, lower-case) corresponding to an integer between 0 and 25.
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- Return the letter (lower-case) got after shifting a letter, (lower-case only),
-- by converting the letter into the corresponding integer, adding on the shift factor and
-- taking the remainder when divided by 26, and converting the resulting
-- integer back into a letter, lower-case. 
-- Do not shift any other characters.
shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let (mod (let2int c + n) 26)
  | otherwise = c

-- Return a string which is encoded by
-- using the shift function and a shift factor.
encodeCaesar :: Int -> String -> String
encodeCaesar n xs = [shift n x | x <- xs]

decodeCaesar :: Int -> String -> String
decodeCaesar n xs = encodeCaesar (-n) xs


-- Cracking the Caesar cipher --

table :: [Float]
table = [
    8.1, 1.5, 2.8, 4.2, 12.7,
    2.2, 2.0, 6.1, 7.0, 0.2,
    0.8, 4.0, 2.4, 6.7, 7.5,
    1.9, 0.1, 6.0, 6.3, 9.0,
    2.8, 1.0, 2.4, 0.2, 2.0, 
    0.1
  ]

-- Return the percentage of a number with respect to another number.
percentageOfIntWrtAnother :: Int -> Int -> Float
percentageOfIntWrtAnother n m = (fromIntegral n / fromIntegral m) * 100

-- Return a frequency table for any given string, by 
-- by using the functions percent.. and numOfChars..
freqs :: String -> [Float]
freqs xs = [percentageOfIntWrtAnother (countOfCharInString x xs) n | x <- ['a'..'z']]
             where n = numOfCharsLowerCase xs 

-- chi-square statistic: Return the value of the chi-square, 
-- by calculating the sum of n ratios of the squares of the differences of the 
-- observed frequencies and the expected frequencies to the expected frequencies.
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

-- Return a list where,
-- each element is rotated n places to the left, wrapping around the start of the
-- list, and assuming that the integer argument n is between zero and the length
-- of the list.
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- Return the decoded string,
-- by finding the shift factor using which the plain text was encoded.
crackCaesar :: String -> String
crackCaesar xs = encodeCaesar (-factor) xs
  where
    factor = head (positionsOfValInList (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs


-- 5.4 String comprehensions --

-- Return the number of lower case characters in a string, by
-- calculating the length of a list whose elements fall between 'a'
-- and 'z', both inclusive.
numOfCharsLowerCase :: String -> Int
numOfCharsLowerCase xs = length [x| x <- xs, x >= 'a' && x <= 'z']

numOfCharsLowerCaseNot :: String -> Int
numOfCharsLowerCaseNot xs = length [x| x <- xs, not (x >= 'a' && x <= 'z')]

uppers :: String -> Int
uppers xs = length [x| x <- xs, x >= 'A' && x <= 'Z']

-- Return the number of times a particular character occurs in a string, by
-- calculating the length of a list of characters that match the desired 
-- character.
countOfCharInString :: Char -> String -> Int
countOfCharInString c cs = length [c' | c' <- cs, c' == c]


-- 5.4 The zip function --

pairsOfAdjacents :: [a] -> [(a,a)]
pairsOfAdjacents xs = zip xs (tail xs)


isSortedInAscending :: Ord a => [a] -> Bool
isSortedInAscending xs = and [x <= y | (x,y) <- pairsOfAdjacents xs]

-- Return a list of all positions at which a value occurs in a list, by 
-- pairing each element with its position, and selecting those positions at which
-- the desired value occurs.
positionsOfValInList :: Eq a => a -> [a] -> [Int]
positionsOfValInList x xs = [i | (x', i) <- zip xs [0..], x' == x]


-- 5.2 Guards --

evensOnly :: [Int]
evensOnly = [n | n <- [1..10], even n]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]



    
-- Basic concepts --

squares = [x^2 | x <- [1..5]]

pairs = [(x,y) | x <- [1..5], y <- [1..3]]
pairs2 = [(x,y) | x <- [1..5], y <- [x..3]]

concat2 :: [[a]] -> [a]
concat2 xss = [x | xs <- xss, x <- xs] 

firsts2 :: [(a,b)] -> [a]
firsts2 ps = [x | (x,_) <- ps]

lengthOfList :: [a] -> Int
lengthOfList xs = sum [1 | _ <- xs]



-- Chapter 4: Defining functions --


-- 4.8 Exercises --

-- 1. Using library functions define a function halve :: [a] -> ([a],[a]) that splits an even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

halve' :: [a] -> ([a], [a])
halve' xs = (take n xs, drop n xs)
              where n = (length xs) `div` 2

halve'' :: [a] -> ([a], [a])
halve'' xs = splitAt n xs
              where n = (length xs) `div` 2

-- 2. Define a function third :: [a] -> a that returns the third element in a list that contains at least this many elements using:
-- a. head and tail;
-- b. list indexing !!;
-- c. pattern matching.

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = xs !! 2

thirdC :: [a] -> a
thirdC (x:x2:x3:xs) = x3

-- 3. Consider a function safetail :: [a] -> [a] that behaves the same way as tail does except that it maps the empty list to itself rather than producing an error.
-- Using tail and the function null :: [a] -> Bool that decides if a list is empty or not, define safetail using:
-- a. a conditional expression;
-- b. guarded equations;
-- c. pattern matching.

-- a. a conditional expression;
safetail_a :: [a] -> [a]
safetail_a xs = 
  if listNull xs
    then []
    else tail xs

-- b. guarded equations;
safetail_b :: [a] -> [a]
safetail_b xs 
  | listNull xs = []
  | otherwise = tail xs

-- c. pattern matching.
safetail_c :: [a] -> [a]
safetail_c [] = []
safetail_c (_:xs) = xs


-- 4. In a similar way to && in section 4.4, show how the disjunction operator || can be defined in four different ways using pattern matching.
or_logical :: Bool -> Bool -> Bool
or_logical False False = False
or_logical False True = True
or_logical True False = True
or_logical True True = True

or_logical2 :: Bool -> Bool -> Bool
or_logical2 False False = False
or_logical2 True _ = True

or_logical3 :: Bool -> Bool -> Bool
or_logical3 False b = b
or_logical3 True b = True

or_logical4 :: Bool -> Bool -> Bool
or_logical4 b b1 
    | b == b1 = b
    | otherwise = True

listNull :: [a] -> Bool
listNull [] = True
listNull xs = False
  

-- 7. Show how the meaning of the following curried functions definition can be formalised in terms of lambda expressions.
multL :: Int -> Int -> Int -> Int
-- multL x y z = x * y * z
multL = \x -> \y -> \z -> x * y * z

-- 8. Define a function luhnDouble::Int->Int that doubles a digit and subtracts 9 if the result is greater than 9.
-- Using luhnDouble and `mod`, define a function luhn::Int->Int->Int->Int->Bool that decides if a four-digit bank card number is valid.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn i j k l 
  | mod (luhnDouble i + j + luhnDouble k + l) 10 == 0 = True
  | otherwise = False


luhnDouble :: Int -> Int
luhnDouble n 
    | n*2 < 9   = n*2
    | otherwise = (n*2 - 9)


-- 4.5 Lambda expressions --

sumL :: [Int] -> Int
sumL = foldl (+) 0

add :: (Int, Int) -> Int
add (x, y) = x + y

addLambda :: (Int, Int) -> Int
addLambda = \(x, y) -> x + y

addLambda' :: Int -> (Int -> Int)
addLambda' x = \y -> x + y

addLambda'' :: Int -> Int -> Int
addLambda'' = \x -> \y -> x + y

constLib :: a -> b -> a
constLib x _ = x

constLibLambda :: a -> (b -> a)
constLibLambda x = \_ -> x

constLibLambda' :: a -> (b -> a)
constLibLambda' = \x -> \_ -> x

odds :: Int -> [Int] -- Takes a number n, and prints a list of n odd numbers.
odds n = map f [0..n-1]
           where f x = x*2 + 1

oddsLamdba :: Int -> [Int] -- Takes a number n, and prints a list of n odd numbers.
oddsLamdba n = map (\x -> x*2 + 1) [0..n-1]

-----------------------

facs :: Integer -> [Integer] -- Takes a positive integer and returns a list of facs of all numbers from zero upto that number.
facs n = map fac [0..n]

fibs :: Int -> [Integer] -- Takes a positive integer and returns a list of the Fribonacci sequences of all numbers from zero upto that number.
fibs n = map fibHut [0..n]


------------------------



-- 4.4 Pattern Matching --

testPatternListWithCons :: [Char] -> Bool
testPatternListWithCons ('a':_) = True
testPatternListWithCons _ = False

testPatternList :: [Char] -> Bool
testPatternList ['a', _, _] = True
testPatternList _ = False

and_logical :: Bool -> Bool -> Bool
and_logical True True = True
and_logical True False   = False
and_logical False True   = False
and_logical False False  = False

and_logical2 :: Bool -> Bool -> Bool
and_logical2 True True = True 
and_logical2 _ _ = False 

and_logical3 :: Bool -> Bool -> Bool
and_logical3 True b = b
and_logical3 False _ = False

and_logical4 :: Bool -> Bool -> Bool
and_logical4 b b1 
    | b == b1 = b
    | otherwise = False



-- 4.3 Guarded equations --

absoluteWithGuards :: Int -> Int
absoluteWithGuards n 
  | n >= 0 = n
  | otherwise = -n

signOfNumUsingGuards :: Int -> Int
signOfNumUsingGuards n 
  | n > 0 = 1
  | n == 0 = 0
  | n < 0 = -1


-- 4.2 Conditional expressions --

absolute :: Int -> Int
absolute n = 
  if n < 0
    then -n
    else n

signOfNum :: Int -> Int
signOfNum n = 
  if n < 0
    then -1
    else 
      if n == 0 
        then 0
        else 1


signOfNumUsingCase :: Int -> Int
signOfNumUsingCase n = 
  case (n < 0) of
    True -> -1
    False ->
      case (n == 0) of 
        True -> 0
        False -> 1

illustrateNoElse :: Int -> Int
illustrateNoElse n = 
  if True
    then 
      (if False
        then 1
        else 2)
    else 0

add2 m n f x = m f ( n f x)

mul m n f x = m (n f) x

getZero = doNever (+1) 0
getOne = doOnce (+1) 0
getTwo = doTwice (+1) 0
getThree = doThreeTimes (+1) 0

isZero :: Int -> Bool
isZero n =
  if n == 0
    then True
    else False

decr :: Int -> Int
decr n = n - 1

----------------------------------------


doNever :: (a -> b) -> a -> a
doNever f x = x

doOnce :: (a -> b) -> a -> b
doOnce f x = f x

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

doThreeTimes :: (a -> a) -> a -> a
doThreeTimes f x = f (f (f x))

----------------------------------------

sumIt []     = 0
sumIt (n:ns) = (+) n (sumIt ns)


qsort :: Ord(a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
       (qsort [a | a <- xs, a <= x]) 
    ++ [x] 
    ++ (qsort [b | b <- xs, b > x])


seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) =
  do x <- act
     xs <- seqn acts
     return (x:xs)

-- Chapter 3: Types and Classes --

-- Exercises 3 --

-- 1 What are the types of the following values?
ex1 = ['a','b','c'] :: [Char]
ex2 = ('a','b','c') :: (Char, Char, Char)
ex3 = [(False, '0'), (True, '1')] :: [(Bool, Char)]
ex4 = ([False, True], ['0', '1']) :: ([Bool], [Char])
ex5 = [tail, init, reverse] :: [[a] -> [a]]

-- 2 Write down definitions that have the following types.
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1,2,3], [2,4,6], [3,6,9]]

exAdd :: Int -> Int -> Int -> Int
exAdd = \x -> \y -> \z -> x+y+z

exCopy :: a -> (a,a)
exCopy x = (x, x)

exApply :: (a -> b) -> a -> b
exApply a b = a b

-- 3 What are the types of the following functions?
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

exDouble :: Num a => a -> a
exDouble x = x*2

palindrome :: String -> Bool
palindrome xs = reverse xs == xs


-- Basic concepts --

zeroto :: Int -> [Int]
zeroto n = [0 .. n]


-- 3.6 Curried functions --

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

incrByOne :: Int -> Int -- Partial Application --
incrByOne n = addLambda' 1 n

incrByOne' :: Int -> Int
incrByOne' n = addLambda'' 1 n



-- Chapter 2: Frirst steps --


-- Exercises 2 --

-- 4. Show how the library function last could be defined in terms of other library functions introduced in this chapter. Can you think of another possible definition?
fnLast1 :: [t] -> t
fnLast1 xs = head (reverse xs)

fnLast2 :: [t] -> t
fnLast2 xs = head (drop (length xs - 1) xs)

fnLast3 :: [t] -> t
fnLast3 xs = xs !! (length xs-1)

-- 5. Show how the library function init could be defined in two different ways.
fnInit1 :: [t] -> [t]
fnInit1 xs = take (length xs - 1) xs

fnInit2 :: [t] -> [t]
fnInit2 xs = reverse (tail (reverse xs))



-- 2.5 Haskell scripts --

fac :: Integer -> Integer
fac n = product [1..n]


-- Exercises 1 --

-- 1. Give another possible calculation for the result of double (double 2).
double x =
  x + x

quadruple x =
  double (double x)

-- 3: Define a function product that produces a product of a list of numbers, and shw using your definition that product [2,3,4] is 24.
product2 []     = 1
product2 (n:ns) = (*) n (product2 ns)

-- 4: How should the definition of qsort be modified so that it produces a reverse sorted version of a list.
qsortReverse :: Ord(a) => [a] -> [a]
qsortReverse [] = []
qsortReverse (x:xs) = 
     (qsortReverse [b | b <- xs, b > x]) 
  ++ [x] 
  ++ (qsortReverse [a | a <- xs, a <= x])


-- 5: What would the effect be of replacing <= with < in the original definition of qsort?

qsort2 :: Ord(a) => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = 
     (qsort [a | a <- xs, a < x]) 
  ++ [x] 
  ++ (qsort [b | b <- xs, b > x])


