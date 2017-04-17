import Data.Char



-- Chapter 1: Basic Concepts --


-- 1.7 Exercises --


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



-- Chapter 2: First steps --

-- 2.7 Exercises --

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



-- Chapter 3: Types and Classes --


-- 3.11 Exercises --


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
sol7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]] -- "2 dimensions"
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


-- Chapter 7: Higher-order functions --

-- 7.9 Exercises --

-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using 
-- the higher-order functions map and filter.
filmap :: [a] -> (a -> Bool) -> (a -> b) -> [b]
filmap xs p f = map f (filter p xs)  


-- 2. Without looking at the definitions from the standard prelude, define the following 
-- higher-order library functions on lists.

-- a. Decide if all elements of a list satisfy a predicate:
allHut :: (a -> Bool) -> [a] -> Bool
allHut p [] = True
allHut p (x:xs) = p x && allHut p xs

allC :: (a -> Bool) -> [a] -> Bool
allC p = and.map p

-- b. Decide if any element of a list satisfies a predicate:
anyHut :: (a -> Bool) -> [a] -> Bool
anyHut p [] = False
anyHut p (x:xs) = p x || anyHut p xs

anyC :: (a -> Bool) -> [a] -> Bool
anyC p = or.map p

-- c. Select elements from a list while they satisfy a predicate.
takeWhileHut :: (a -> Bool) -> [a] -> [a]
takeWhileHut _ [] = []
takeWhileHut p (x:xs) | p x       = x : takeWhileHut p xs
                      | otherwise = []

-- d. Reject elements from a list while they satisfy a predicate.
dropWhileHut :: (a -> Bool) -> [a] -> [a]
dropWhileHut _ [] = []
dropWhileHut p (x:xs) | p x       = dropWhileHut p xs
                      | otherwise = x : xs


-- 3. Redifine the functions map f and filter p using foldr.
mapFr, mapFr2, mapR :: (a -> b) -> [a] -> [b]

mapFr f = foldr ((:) . f) []

mapFr2 f = foldr (\x -> \xs -> f x : xs) []

mapR _ [] = []
mapR f (x:xs) = f x : mapR f xs 

filterFr :: (a -> Bool) -> [a] -> [a]
filterFr p = foldr (\x xs -> if p x then x : xs else xs) []  

filterR :: (a -> Bool) -> [a] -> [a]
filterR _ [] = []
filterR p (x:xs) | p x       = x : filterR p xs
                 | otherwise = filterR p xs


-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts
-- a decimal number into an integer. E.g. dec2int [2,3,4,5] = 2345

-----------------------------------------------------------------------------

-- f v []     = v
-- f v (x:xs) = f (v # x) xs

-- foldl (#) v [x0, x1, ... , xn] = (... ((v # x0) # x1) ...) # xn

-----------------------------------------------------------------------------

dec2int :: [Int] -> Int
dec2int = foldl (\v -> \x -> 10*v + x) 0


-- 5. Without looking at the definitions in the standard prelude, define the 
-- higher-order library function curry that converts a function on pairs to 
-- a curried function, and conversely
-- the function uncurry that converts a curried function with two arguments into 
-- a function on pairs.
-- Hint: first write down the types of the two functions.

curryHut :: ((a,b) -> c) -> a -> b -> c
curryHut f = \x -> \y -> f (x,y)

uncurryHut :: (a -> b -> c) -> (a,b) -> c
uncurryHut f = \(x,y) -> f x y


-- 6. A higher-order function unfold that encapsulates a simple pattern of
-- recursion for producing a list can be defined as follows:
unfoldHut p h t x | p x       = []
                  | otherwise = h x : unfoldHut p h t (t x)
-- Redefine the functions chop8, map f and iterate f using unfold.

-- Convert an integer to list of bits 0 or 1.
int2binUnf :: Int ->[Bit]
int2binUnf = unfoldHut (== 0) (`mod` 2) (`div` 2)

--             chop8 :: [Bit] -> [[Bit]]
--             chop8 []   = []
--             chop8 bits = take 8 bits : chop8 (drop 8 bits) 

chop8Unf :: [Bit] -> [[Bit]]
chop8Unf = unfoldHut (== []) (take 8) (drop 8) 

mapUnf :: Eq a => (a -> b) -> [a] -> [b]
mapUnf f = unfoldHut (== []) (f . head ) (tail )

iterateHut :: (Int -> Int) -> Int -> [Int]
iterateHut f n | n > 144  = []
               | otherwise = n : iterateHut f (f n)

iterateUnf :: (Int -> Int) -> Int -> [Int]
iterateUnf f = unfoldHut (> 144) id f

-- Iterate while True
iterateUnfBTrue :: (Bool -> Bool) -> Bool -> [Bool]
iterateUnfBTrue f = unfoldHut (== False) id f

-- Iterate while False
iterateUnfBFalse :: (Bool -> Bool) -> Bool -> [Bool]
iterateUnfBFalse f = unfoldHut (== True) id f


-- 7. Modify the binary string transmitter example to detect simple transmission 
-- errors using the concept of parity bits. That is, each 8-bit binary number
-- produced during encoding is extended with a parity bit. The parity bit is set 
-- to 1 if the number contains an odd number of ones, and to 0 otherwise. 
-- In turn, each resulting 9-bit number consumed during decoding is checked to 
-- ensure that its parity bit is correct. If the parity bit is correct it is 
-- discarded, otherwise a parity error is reported.
--
-- Hint: the library function error :: String -> a displays the given string as
-- an error message and terminates the program; the polymorphic result type 
-- ensures that error can be used in any context.
parify :: [Int] -> [Int]
parify [] = []
parify xs | even (sum xs) = xs ++ [0]
          | otherwise     = xs ++ [1]

unparify :: [Int] -> [Int]
unparify [] = []
unparify xs | even (sum (init xs)) && (last xs == 0) = init xs
            | odd (sum (init xs)) && (last xs == 1)  = init xs
            | otherwise                              = error "Parity error!"


-- 8. Test your new string transmitter program from the previous exercise using
-- a faulty communication channel that forgets the first bit, which can be 
-- modelled using the tail function on lists of bits.
transmitParity :: String -> String
transmitParity = decode . unparify . channelParity . parify . encode
             where
               channelParity :: [Bit] -> [Bit]
               channelParity = tail


-- 9. Define a function thta applies its 2 argument functions to successive
-- elements in a list alternately, in turn about order.
-- For example:
-- altMap (+10) (+100) [0,1,2,3,4]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ (x0:[]) = f x0 : []
altMap f g (x0:x1:[]) = f x0 : g x1 : []
altMap f g (x0:x1:xs) = f x0 : g x1 : altMap f g xs


-- 10. Using altMap, define a function luhn that implements the Luhn algorithm from
-- the exercises in chapter 4 for bank card numbers of any length. 
-- Test your new function using your own bank card.

{- 
-- 8. The Luhn algorithm is used to check bank card numbers from simple errors such as
-- mistyping a digit, and proceeds as follows:
-- . consider each digit as a seperate number;
-- . moving left, double every other number from the second last;
-- . subtract 9 from each number that is now greater than 9;
-- . add all the resulting numbers together;
-- . if the total is divisible by 10, the card number is valid.
--
-- Define a function luhnDouble::Int->Int that doubles a digit and subtracts 9 
-- if the result is greater than 9.
-- Using luhnDouble and `mod`, define a function luhn that decides if a four-digit bank 
-- card number is valid.

luhn :: Int -> Int -> Int -> Int -> Bool
luhn i j k l 
  | mod (luhnDouble i + j + luhnDouble k + l) 10 == 0 = True
  | otherwise = False


luhnDouble :: Int -> Int
luhnDouble n 
    | n*2 < 9   = n*2
    | otherwise = (n*2 - 9)
-}

luhn2 :: [Int] -> Bool
luhn2 ns | mod ((sum . altMap id luhnDouble . reverse) ns) 10 == 0 = True
         | otherwise = False



