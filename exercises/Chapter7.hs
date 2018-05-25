{-
  Chapter7.hs

  Solutions of exercises.

  Chapter 7: Higher-order functions
  Programming in Haskell, 2nd Edition, by
  Graham Hutton.
-}

import Data.Char

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
