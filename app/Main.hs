{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Allows automatically deriving newtypes
{-# OPTIONS_GHC -Wdefault #-} -- there is also -Wall and -Weverything, which is the harshest, -Werrors 

module Main where

{-
  Imports on top
-}

import Lib
import Data.Function
import Data.List
import Debug.Trace

-- pattern matching

isPre [] _          = True
isPre _ []          = False
isPre (x:xs) (y:ys) = x == y && isPre xs ys

isPre' xs ys = case (xs, ys) of
    ([], _)      -> True
    (_, [])      -> False
    (x:xs, y:ys) -> x == y && isPre' xs ys


-- declaration where or let
f = let x = 1; y = 2 in (x+y)
g = x+y where x=1; y=1

h =
    let x = 1
        y = 2
    in x + y

k = x + y
    where
        x = 1
        y = 1
    
-- pattern guards, you can mix boolean and patterns
isPre''' xs ys
    | [] <- xs         = True
    | [] <- ys         = False
    | x:xs' <- xs,
      y:ys' <- ys     = x == y && isPre''' xs' ys'

-- functio composition & invocation syntaxes
ex0 = (filter (<100) . map (*20)) [1,2,3]
ex1 = filter (<100) . map (*20) $ [1,2,3]

ex2 = [1,2,3] |> map (*20) |> filter (<100)
ex3 = [1,2,3] & map (*20) & filter (<100) -- from Data.Function

x |> f = f x -- F# syntax

sortSize = sortBy (compare `on` length)
sorted   = sortSize [[1,2], [1,2,3], [1]]

-- list comprehension, express in the right order to minimize iterations
compr = [(x',y',z) | x <- [1,2], let x' = x + 1, y <- [3,4], let y' = y * 2, odd x', z <- [5,6]]
nums = [1,3..21]
letters = ['a'..'z']

factorial n = product [1..n]

-- infinite list
powersOfTwo = iterate (2*) 1

-- operators as functions
n = (+1) 3 + (+4) 2

-- type classes
class Equal a where
  equal :: a -> a -> Bool

instance Equal Bool where
  equal True True   = True
  equal False False = True
  equal True False  = False
  equal False True  = False

instance Equal () where
  equal () () = True

instance (Equal a) => Equal [a] where
  equal [] [] = True
  equal [] ys = False
  equal xs [] = False
  -- equal x y is only allowed here due to the constraint (Equal a)
  equal (x:xs) (y:ys) = equal x y && equal xs ys

instance Equal Int where
    equal n1 n2 = n1 == n2

instance Equal Char where
    equal n1 n2 = n1 == n2

instance (Equal a, Equal b) => Equal (a,b) where
  equal (x0, x1) (y0, y1) = equal x0 y0 && equal x1 y1

tinstance = [(1::Int, "bob"), (3::Int, "rob")] `equal` [(1::Int, "bob"), (3::Int, "rob")]

-- automatic derivation
data MyList a = Cons a (MyList a) | Nil deriving (Eq, Ord, Show)
myL1 = Cons 1 (Cons 2 Nil)
myL2 = Cons 1 (Cons 3 Nil)

-- records
data Person = Person { name :: String, age :: Int }
p1 = Person { name = "bob", age = 10 }
p2 = Person "rob" 20
p3 = p1 { age = 50 }

-- ugly syntax
p1Name = name p1
p2Name = p2&name

-- sum of product generate partial functions
data Example = Ex1 { a :: Int } | Ex2 { b :: Int }
egl = Ex1 34
wrong = egl&b -- Exception when printed

-- lack of namespacing -- below doesn't compile as 'a' was defined above
-- data Example1 = Exx1 { a :: Int, b1 :: Int }

bigName = case p1 of
    Person { name = "bob" } -> "BIG"
    _ -> "SMALL"

-- newtype
newtype Money = Money Double
    deriving (Show, Eq, Ord, Num)

-- If not GeneralizedNewtypeDeriving, then you need to write the code below
{-
instance Num Money where
   fromInteger = Money . fromIntegral
   Money x + Money y = Money (x + y)
-}

money1 = Money 32.0
money2 = Money 64
money3 = money1 + money2

-- below doesn't type check
-- money4 = money1 + 3.2

-- using undefined to partially define functions, error for bailing out, you get runtime errors
addThreeNums n m j         = undefined
divByY _ 0                 = error "Divide by zero error"
divByY dividend divisor    = dividend / divisor

-- partial functions are allowed, but the linter complains, to make it a warning do:
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- unsafe (Just x) = Just $ x + 1

-- But no linter or warning if lambda unless -fwarn-incomplete-uni-patterns
takeLambda f  = f 
result = takeLambda (\(Just a) -> 1) Nothing

-- you can trace in the code, but it is impure!!
example3 = [trace "called when used" 3]

-- type hole are useful for debugging, put '_' instead of a value or type
-- head' = head _
const' :: _
const' x y = x

main :: IO ()
main = do
    print ("Bob" `isPre` "Bobby")
    print ("Bob" `isPre'` "Bobby")
    print ("Bob" `isPre'''` "Bobby")
    print $ take 5 powersOfTwo
    print n
    print $ ex0 == ex1 && ex1 == ex2 && ex2 == ex3
    print sorted
    print compr
    print nums
    print letters
    print $ factorial 3
    print tinstance
    print myL1
    print $ myL2 > myL1
    print money3
    print example3
