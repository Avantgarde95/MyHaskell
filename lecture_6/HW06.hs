{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 (tail fibs2))

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = (Cons x (sRepeat x))

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = (Cons x (sIterate f (f x)))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = (Cons x (sInterleave y xs))

-- helper function for sTake...
sliceList xs from to = take (to - from + 1) (drop from xs)

sTake :: Int -> Stream a -> [a]
sTake n x = (sliceList (streamToList x) 0 (n - 1))

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = (sIterate (+ 1) 0)

ruler :: Stream Integer
ruler = foldr1 sInterleave (map sRepeat [0..])

-- Exercise 7 -----------------------------------------

randRec r = (1103515245 * r + 12345) `mod` 2147483648

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r = (sIterate randRec r)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax (x : xs) = Just (a, b) where
    (a, b) = foldr (\x (x_min, x_max) -> (min x x_min, max x x_max)) (x, x) xs

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
