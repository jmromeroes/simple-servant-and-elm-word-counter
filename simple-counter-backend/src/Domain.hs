{-# LANGUAGE DeriveGeneric #-}

module Domain where

import Data.Aeson hiding (decode)
import Data.List (sortBy)
import Data.Map (Map)

import qualified Data.Map as Map

import GHC.Generics

-- * data

-- This type is to define the order of the word count result
data SortBy = Asc | Desc deriving (Bounded, Show, Enum)

-- Result type
data WordCount
  = WordCount {
    word :: String,
    count :: Integer
  } deriving (Eq, Show, Generic)

-- Intances necessary for mapping the WordCount type to/from json
instance FromJSON WordCount
instance ToJSON WordCount

-- * core

-- Maps from the map association to the data type that we reutnr

assocToWordCount :: (String, Integer) -> WordCount
assocToWordCount (word, count) = WordCount word count

-- From a splitted string creates a Map with the word and count association
countWordsMap :: [String] -> Map String Integer -> Map String Integer
countWordsMap [] m = m 
countWordsMap (x:xs) m =
  let
    v = Map.findWithDefault 0 x m
  in
    countWordsMap xs (Map.insert x (v + 1) m)

-- This is the usual way to create orderings in Haskell, using the types LT (Lower than) and GT (Greater than)
compareWordCount :: WordCount -> WordCount -> Ordering
compareWordCount (WordCount _ countA) (WordCount _ countB)
  | countA < countB = LT
  | otherwise       = GT

-- This is just a way to retrieve a sorted list according to the SortBy specific type

sortArray :: [WordCount] -> Maybe SortBy -> [WordCount]
sortArray wordsCount (Just Asc)  = sortBy compareWordCount wordsCount
sortArray wordsCount (Just Desc) = reverse $ sortBy compareWordCount wordsCount
sortArray wordsCount Nothing     = wordsCount

-- Using the helper functions this creates and sorts the WordCount list
countWords :: String -> Maybe SortBy -> [WordCount]
countWords text sortBy_ =
  let
    splitted = map (filter (/='"')) $ words text
  in
    map assocToWordCount $ Map.assocs (countWordsMap splitted  Map.empty)
