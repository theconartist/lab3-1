--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 3: Recursive and higher-order functions                                --
--------------------------------------------------------------------------------

module Lab3 where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Monoid(..), elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

import Data.List (delete)
--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem t [] = False
elem t (x:xs) = t == x || elem t xs

elem' :: Eq a => a -> [a] -> Bool
elem' t xs = not(null(filter (==t) xs))

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = if x > restMax then x else restMax
    where restMax = maximum xs

maximum' :: Ord a => [a] -> a
maximum' xs = foldr1 (\x y -> if x > y then x else y) xs 

intersperse :: a -> [a] -> [a]
intersperse sep [x] = [x] 
intersperse sep (x:xs) = [x] ++ [sep] ++ intersperse sep xs 

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' t xs = any (==t) xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = f x && all f xs

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs)
    |f x == True = x : takeWhile f xs
    |otherwise = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = undefined

subsequences :: [a] -> [[a]]
subsequences = undefined

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (delete x xs)]

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mappend mempty x = x
-- (Right identity)     mappend x mempty = x
-- (Associativity)      mappend x (mappend y z) = mappend (mappend x y) z
-- (mconcat)            mconcat = foldr mappend mempty

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = undefined

instance Monoid Int where
    mempty  = undefined
    mappend = undefined

instance Monoid [a] where
    mempty  = undefined
    mappend = undefined

instance Monoid b => Monoid (a -> b) where
    mempty  = undefined
    mappend = undefined

--------------------------------------------------------------------------------
