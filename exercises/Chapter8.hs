{-
  Chapter8.hs

  Solutions of exercises.

  Chapter 8: Declaring types and classes
  Programming in Haskell, 2nd Edition, by
  Graham Hutton.
-}

import Data.Char

{-

1. In a similar manner to the function add, define a recursive multiplication
   function for the recursive type of natural numbers. 
   Hint: make use of addNats in your definition.

addNatsR :: Nat -> Nat -> Nat
addNatsR Zero n     = n
addNatsR (Succ m) n = Succ (addNatsR m n) 

-}

multNat :: Nat -> Nat -> Nat
multNat m n = int2nat (nat2int m * nat2int n)

multNatsR            :: Nat -> Nat -> Nat
multNatsR Zero n     = Zero
multNatsR n Zero     = Zero
multNatsR (Succ m) n = addNatsR n (multNatsR m n) 


{-
2. Although not included in appendix B, the standard prelude defines data 
   Ordering = LT | EQ | GT together with a function
   compare :: Ord a => a -> a -> Ordering that decides if one value in an 
   ordered type is less than (LT), equal to (EQ), or greater than (GT) another value. 
   Using this function, redefine the function 
   occurs :: Ord a => a -> Tree a -> Bool for search trees.
   Why is this new definition more efficient than the original version?

occursTSearch                          :: (Ord a) => a -> Tree a -> Bool
occursTSearch x (Leaf y)               =  x == y
occursTSearch x (Node l y r) | x == y  =  True
                             | x < y   =  occursTSearch x l
                             | x > y   =  occursTSearch x r

-}

occursTSearchOrd                :: (Ord a) => a -> Tree a -> Bool
occursTSearchOrd x (Leaf y)     =  compare x y == EQ
occursTSearchOrd x (Node l y r) =
  case compare x y of
    EQ -> True
    LT -> occursTSearchOrd x l
    GT -> occursTSearchOrd x r

{-

3. Consider the following type of binary trees:
   data Tree a = Leaf a | Node (Tree a) (Tree a)
   Let us say that such a tree is 'balanced' if the number of leaves in the left
   and right subtree of every node differs by at most one, with leaves themselves
   being trivially balanced.
   Define a function balanced :: Tree a -> Bool that decides whether a tree is
   balanced or not.
  Hint: first define a function that returns the number of leaves in a tree.

-}

-- Depicts a tree data structure which is balanced.
data TreeBal a = LeafBal a | NodeBal (TreeBal a) (TreeBal a) deriving Show

tBal :: TreeBal Int
tBal = NodeBal  (NodeBal (LeafBal 1) (LeafBal 4)) 
                (NodeBal (LeafBal 6) (LeafBal 9)) 

tBal2 :: TreeBal Int
tBal2 = NodeBal 
            (NodeBal 
                (LeafBal 4) 
                (NodeBal 
                    (LeafBal 13) 
                    (LeafBal 0)))
            (NodeBal 
                (NodeBal 
                    (LeafBal 333) 
                    (NodeBal 
                        (LeafBal 6) 
                        (LeafBal 9)))
                (LeafBal 42))


isBalanced                                 :: TreeBal Int -> Bool
isBalanced (LeafBal _)                     =  True
isBalanced (NodeBal l r) = (diffLeaves <= 1) && isBalanced l && isBalanced r
  
  where

    diffLeaves = abs (numLeaves l - numLeaves r)

    numLeaves               :: TreeBal Int -> Int
    numLeaves (LeafBal _)   =  1
    numLeaves (NodeBal l r) =  numLeaves l + numLeaves r

{- 
 
4. Define a function balance :: [a] -> Tree a that converts a non-empty list into
   a balanced tree.
   Hint: first define a function that splits a list into two halves whose length 
   differs by at most one.

   -- Depicts a tree data structure which is balanced.
   data TreeBal a = LeafBal a | NodeBal (TreeBal a) (TreeBal a) deriving Show

  flatten :: Tree a -> [a]
  flatten (Leaf x) = [x]
  flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-}

balance     :: [a] -> TreeBal a
balance []  =  error "Provide a list that is non-empty!"
balance [x] =  LeafBal x
balance xs  =  NodeBal (balance (fst (halveList xs))) (balance (snd (halveList xs)))  

halveList :: [a] -> ([a],[a])
halveList [] = ([],[])
halveList xs = splitAt (half xs) xs
  where
    half :: [a] -> Int
    half xs = div (length xs) 2 


{-

5. Given the type declaration
   data Expr = Val Int | Add Expr Expr
   Define the higher-order function
   folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
   such that folde f g replaces each Val constructor in an expression by the 
   function f, and each Add constructor by the function g.

-}

folde                 :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)     =  f n
folde f g (Add e1 e2) =  g (folde f g e1) (folde f g e2)


{-
6. Using folde define a funciton
   eval :: Expr -> Int
   that evaluates an expression to an integer value, and a function 
   size :: Expr -> Int
   that calculates the number of values in an expression.
-}

evalE             :: Expr -> Int
evalE (Val n)     =  n
evalE (Add e1 e2) =  folde toEnum (+) (Add e1 e2)

numVals             :: Expr -> Int
numVals (Val n)     =  1
numVals (Add e1 e2) =  numVals e1 + numVals e2  


{-

7. Complete the following instance declarations:
     instance Eq a => Eq (Maybe a) where
     instance Eq a => Eq [a] where

instance Eq a => Eq (Maybe a) where
  -- Defines the (==) operation.
  Nothing == Nothing = True
  Just    == Just    = True
  _       == _       = False

instance Eq a => Eq [a] where
  -- Defines the (==) operation.
  [] == []         = True
  [x] == [y]       = x == y
  (x:xs) == (y:ys) = x==y && xs==ys
  _  == _          = False 

-}


{-

8. Extend the tautology checker to support the use of logical disjunction (V) and
   equivalence (<=>) in propositions.

   Truth Table for Disjunction (OR operation).

    A | B | A v B
   ---------------
    F | F |   F
    F | T |   T
    T | F |   T
    T | T |   T


    Truth Table for Equivalence.

    A | B | A <=> B
   -----------------
    F | F |    T
    F | T |    F
    T | F |    F
    T | T |    F
-}

data Prp =
    Constantification Bool
  | Assignment Char
  | Negation Prp
  | Equivalence Prp Prp
  | Disjunction Prp Prp
  | Conjunction Prp Prp
  | Implication Prp Prp

-- A list of pairs of keys to their corresponding values. 
type Matches key value = [(key,value)]

-- Substitution of values of type Bool for keys of type Char. 
-- E.g. the substitution [('A',False), ('B', True)] substitutes the value
-- False for A and the value True for B.
type Substitution = Matches Char Bool


-- Evaluates a proposition in terms of Substitution for its variables.
evalPrp :: Substitution -> Prp -> Bool
evalPrp _ (Constantification b)  =  b
evalPrp s (Equivalence p1 p2)    =  not (evalPrp s p1 || evalPrp s p2)
evalPrp s (Assignment c)         =  findInTable c s -- Look up the list of Substitution, and substitute the Bool value for the Char argument.
evalPrp s (Negation p)           =  not (evalPrp s p)
evalPrp s (Disjunction p1 p2)    =  evalPrp s p1 || evalPrp s p2
evalPrp s (Conjunction p1 p2)    =  evalPrp s p1 && evalPrp s p2
evalPrp s (Implication p1 p2)    =  evalPrp s p1 <= evalPrp s p2  -- Knowing that: False < True

-- Returns all the variables in a proposition.
variables :: Prp -> [Char]
variables (Constantification _)  =  [] 
variables (Assignment c)        =  [c]
variables (Equivalence p1 p2)   =  variables p1 ++ variables p2
variables (Negation p)          =  variables p
variables (Disjunction p1 p2)   =  variables p1 ++ variables p2
variables (Conjunction p1 p2)   =  variables p1 ++ variables p2
variables (Implication p1 p2)   =  variables p1 ++ variables p2

-- Returns all the possible boolean values.
boolsAll :: Int -> [[Bool]]
boolsAll 0  =  [[]]
boolsAll n  =  map (False :) bss ++ map (True :) bss
                 where
                   bss = boolsAll (n-1)

-- Generates all the possible substitutions for a given proposition.
substitutions :: Prp -> [Substitution]
substitutions p  =  map (zip vs) (boolsAll (length vs))
                    where
                      vs = rmdups $ variables p

-- Decides whether a proposition is a tautology (a logical statement which is always true).
isItATautology :: Prp -> Bool
isItATautology p  =  and [evalPrp s p | s <- substitutions p]



{- 

  9. Extend the abstract machine to support the use of multiplication.

-}

-- Declarations -----------------------------------------------------

data Exp =
    Constantization Int
  | Addition Exp Exp
  | Multiplication Exp Exp
  deriving Show

data Oper = 
    EVALUATE Exp
  | AD Int
  | MU Int

type Controls = [Oper]

---------------------------------------------------------------


-- Definitions

-- Evaluates an expression in the context of a control stack.
evalExp                           :: Exp -> Controls -> Int
evalExp (Constantization n) c     =  execOper c n
evalExp (Addition e1 e2) c        =  evalExp e1 (EVALUATE e2 : AD 0 : c)
evalExp (Multiplication e1 e2) c  =  evalExp e1 (EVALUATE e2 : MU 1 : c)


-- Executes the control stack in the context of an integer operand.
execOper                             :: Controls -> Int -> Int
execOper [] n                        =  n
execOper (EVALUATE e1 : AD 0 : c) n  =  evalExp e1 (AD n : c)
execOper (EVALUATE e1 : MU 1 : c) n  =  evalExp e1 (MU n : c)
execOper (AD n : c) m                =  execOper c (n+m)
execOper (MU n : c) m                =  execOper c (n*m)


-- Calculates the value of an expression using a control stack.
valExp    :: Exp -> Int
valExp e  =  evalExp e []

--

-- Values

exp0 = Constantization 0

exp1 = Constantization 1

exp2 = Addition exp0 exp1

exp3 = Multiplication (Constantization 5) (Constantization 5)

exp4 = Addition exp0 exp2

exp5 =
  Addition
    (Multiplication exp0 exp1)
    (Multiplication exp1 exp2)
