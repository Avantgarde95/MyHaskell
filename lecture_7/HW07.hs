{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Function (on)
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f v = v >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = (liftM2 (\e1 e2 -> v // [(i1, e2), (i2, e1)]) `on` (v !?)) i1 i2

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = foldr (\a b -> ((:) <$> f a <*> b)) (return [])

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM (v !?) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, length v) >>= return . (v !?)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = replicateM len getRandom >>= return . V.fromList

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len (lo, hi) = replicateM len (getRandomR (lo, hi)) >>= return . V.fromList

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = go (length v - 1) v
  where go 0 acc = return acc
        go n acc = getRandomR (0, n) >>= go (n-1) . swapUnsafe acc n
        swapUnsafe vec i1 i2 = vec // [(i1, vec ! i2), (i2, vec ! i1)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec index = (front, (vec ! index), back)
                        where (front, back) = (V.partition (\x->x < (vec ! index)) newvec)
                              newvec = ((V.slice 0 index vec) V.++ (V.slice (index+1) ((V.length vec)-index-1) vec))

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec
    | vec == V.empty = V.empty
    | otherwise = (qsort front) V.++ (V.singleton pivot) V.++ (qsort back)
                  where (front, pivot, back) = partitionAt vec 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec
    | vec == V.empty = do {return V.empty}
    | otherwise = do
                    r <- getRandomR (0, (length vec) - 1)
                    let (front, pivot, back) = partitionAt vec r
                    frontV <- qsortR front
                    backV <- qsortR back
                    let result = frontV V.++ (V.singleton pivot) V.++ backV
                    return result

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank vec
    | V.null vec = do {return Nothing}
    | ((rank < 0) || (rank >= (length vec))) = do {return Nothing}
    | otherwise = do
                    r <- getRandomR (0, (length vec)-1)
                    let (front, pivot, back) = partitionAt vec r
                    if ((length front) > rank)
                      then select rank front
                      else (if ((length front) == rank)
                              then return $ Just pivot
                              else select (rank - ((length front)+1)) back)

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suit | suit <- suits, label <- labels]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
    | V.null deck = Nothing
    | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck
    | n == 0 = Just ([], deck)
    | otherwise = do
                    (card, remn) <- nextCard deck
                    (cards, remDeck) <- getCards (n-1) remn
                    return $ (card : cards, remDeck)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
