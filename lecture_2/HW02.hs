{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys)
  | x == y = 1 + (exactMatches xs ys)
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map countOne colors
    where countOne :: Peg -> Int
          countOne p = sum (map isEqual c)
              where isEqual :: Peg -> Int
                    isEqual a
                      | a == p = 1
                      | otherwise = 0

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sumMin (countColors a) (countColors b)
    where sumMin :: [Int] -> [Int] -> Int
          sumMin [] [] = 0
          sumMin (x:xs) (y:ys)
            | x < y = x + (sumMin xs ys)
            | otherwise = y + (sumMin xs ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b (exactMatches a b) ((matches a b) - (exactMatches a b)) 

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move a e n) b = (Move a e n) == (getMove b a)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m a = filter (isConsistent m) a

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n <= 0 = []
  | n == 1 = [[x] | x <- colors]
  | otherwise = [(x:y) | x <- colors, y <- allCodes (n - 1)]

-- Exercise 7 -----------------------------------------

getRemains :: Code -> [Code] -> (Move, [Code])
getRemains a (b:bs) = (m, filterCodes m bs)
    where m = getMove a b

roundSolve :: Code -> ([Move], [Code]) -> ([Move], [Code])
roundSolve a (ms_prev, c_prev) = ((ms_prev ++ [m]), c)
    where (m, c) = getRemains a c_prev

solveRec :: Code -> ([Move], [Code]) -> ([Move], [Code])
solveRec a (ms, c)
  | c == [] = (ms, c)
  | otherwise = solveRec a (roundSolve a (ms, c))

solve :: Code -> [Move]
solve a = ms_final
    where (ms_final, c_final) = solveRec a ([], (allCodes (length a)))

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess a = [Move [Red] 0 0]
