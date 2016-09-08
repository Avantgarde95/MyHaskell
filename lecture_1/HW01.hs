{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------

-- Break the number into a list of digits in reverse order
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (lastDigit n) : (toRevDigits (dropLastDigit n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : (y : zs)) = x : ((y * 2) : (doubleEveryOther zs))

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
-- ... Very ineffective... I just wanted to avoid using sum().
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : ys)
  | x < 10 = x + (sumDigits ys)
  | otherwise = (lastDigit x) + (sumDigits ((dropLastDigit x) : ys))

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (mod (sumDigits (doubleEveryOther (toRevDigits n))) 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = (hanoi (n - 1) a c b) ++ ([(a, b)] ++ (hanoi (n - 1) c b a))

-- Exercise 7 -----------------------------------------

-- Towers of Hanoi for four pegs
hanoi_4 :: Integer -> Peg -> Peg -> Peg -> Peg -> IO ()
hanoi_4 n a b c d
  | n <= 0 = putStrLn "[]"
  | otherwise = putStrLn "[I don't know T.T]"
