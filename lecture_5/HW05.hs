{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- My imports ----------------------------------------

import Data.Maybe
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (unpackBytes)
import Data.Bits (xor)
import Data.List (sortBy)

strToWord8 = unpackBytes . C8.pack
strToBS = BS.pack . strToWord8

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fname_ori fname_mod = do
    file_ori <- BS.readFile fname_ori
    file_mod <- BS.readFile fname_mod
    return (BS.pack (filter (\x -> x /= 0) (BS.zipWith xor file_ori file_mod)))

-- Exercise 2 -----------------------------------------

sliceList :: [a] -> Int -> Int -> [a] 
sliceList xs from to = take (to - from + 1) (drop from xs)

repeatList :: [a] -> Int -> [a]
repeatList ls num
  | num == 0 = []
  | otherwise = ls ++ (repeatList ls (num - 1))

fillList :: [a] -> [a] -> [a]
fillList part whole = (repeatList part num_repeat)
                      ++ (sliceList part 0 (len_remain - 1)) where
    num_repeat = (length whole) `div` (length part)
    len_remain = (length whole) `mod` (length part)

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key fname = do
    file_enc <- BS.readFile (fname ++ ".enc")
    let key_filled = BS.pack (fillList (BS.unpack key) (BS.unpack file_enc))
    let data_decrypted = BS.pack (BS.zipWith xor key_filled file_enc)
    BS.writeFile fname data_decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fname = do
    file_json <- BS.readFile fname
    return (decode file_json)

-- Exercise 4 -----------------------------------------

isBadTs :: [TId] -> Transaction -> Bool
isBadTs victims trans = elem (tid trans) victims

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fname_victims fname_trans = do
    data_victims <- parseFile fname_victims
    data_trans <- parseFile fname_trans
    return (Just (filter (isBadTs (fromJust data_victims)) (fromJust data_trans)))

-- Exercise 5 -----------------------------------------

addToMap :: String -> Integer -> Map String Integer -> Map String Integer
addToMap key value mapping
  | (Map.member key mapping) = (Map.insert key (value + (fromJust (Map.lookup key mapping))) mapping)
  | otherwise = (Map.insert key value mapping)

getFlow :: [Transaction] -> Map String Integer
getFlow [] = (Map.fromList [])
getFlow (x:xs) = (addToMap (to x) (amount x) (addToMap (from x) (0 - (amount x)) (getFlow xs)))

-- Exercise 6 -----------------------------------------

getMaxTrans :: [(String, Integer)] -> (String, Integer)
getMaxTrans [(key, value)] = (key, value)
getMaxTrans (x:xs)
  | value > value_best = (key, value)
  | otherwise = (key_best, value_best)
  where
      (key, value) = x
      (key_best, value_best) = (getMaxTrans xs)

getCriminal :: Map String Integer -> String
getCriminal mapping = key where
    (key, value) = getMaxTrans (Map.toList mapping)

-- Exercise 7 -----------------------------------------

-- phase 1

getPayers :: Map String Integer -> [(String, Integer)]
getPayers mapping = (filter goodPeople (Map.toList mapping)) where
    goodPeople :: (String, Integer) -> Bool
    goodPeople (key, value) = value >= 0

getPayees :: Map String Integer -> [(String, Integer)]
getPayees mapping = (filter poorPeople (Map.toList mapping)) where
    poorPeople :: (String, Integer) -> Bool
    poorPeople (key, value) = value < 0

-- phase 2

comparePeople :: (String, Integer) -> (String, Integer) -> Ordering
comparePeople (key1, value1) (key2, value2)
  | abs(value1) < abs(value2) = GT
  | abs(value1) > abs(value2) = LT
  | otherwise = EQ

sortGroup :: [(String, Integer)] -> [(String, Integer)]
sortGroup group = sortBy comparePeople group

-- phase 3

updateTrans :: [(String, Integer)] -> [(String, Integer)] -> [Transaction] -> [TId] -> [Transaction]
updateTrans [] [] trans tids = trans
updateTrans [] payees trans tids = trans
updateTrans payers [] trans tids = trans
updateTrans ((p1, v1):xs) ((p2, v2):ys) trans (t:ts)
  | v1 > (0 - v2) = updateTrans ((p1, (v1 + v2)):xs) ys ((Transaction p1 p2 (0 - v2) t):trans) ts
  | v1 < (0 - v2) = updateTrans xs ((p2, (v1 + v2)):ys) ((Transaction p1 p2 v1 t):trans) ts
  | otherwise = updateTrans xs ys ((Transaction p1 p2 v1 t):trans) ts

-- main

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mapping tids = updateTrans payers payees [] tids where
    payers = (sortGroup (getPayers mapping))
    payees = (sortGroup (getPayees mapping))

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fname data_json = BS.writeFile fname (encode data_json)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

