{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client.MultipartFormData
import Servant
import Servant.Multipart
import Servant.API
import System.IO
import Web.HttpApiData (parseBoundedTextData)
import Control.Monad.IO.Class
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as LBS
          
-- * api

-- This is the API definition
type WordsCountAPI =
  "words" :> "count" :> MultipartForm Mem (MultipartData Mem) :> QueryParam "sortBy" SortBy :> Post '[JSON] [WordCount]
  
wordsApi :: Proxy WordsCountAPI
wordsApi = Proxy

-- * app

-- This is the main function, we set some settings (e.g port, print a string in the terminal, and the defaultSettings that Warp provides) and then we run the server, injecting this settings
run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

-- Creates IO application from API definition and serrver
mkApp :: IO Application
mkApp = return $ serve wordsApi server

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: Server WordsCountAPI
server = countWordsApi

-- Maps from the map association to the data type that we reutnr
assocToWordCount :: (String, Integer) -> WordCount
assocToWordCount (word, count) = WordCount word count

-- From a splitted string creates a Map with the word and count association
countWordsMap :: [String] -> Map String Integer -> Map String Integer
countWordsMap [] m = m 
countWordsMap (x:xs) m =
  let
    v = Map.findWithDefault 1 x m
  in
    countWordsMap xs (Map.insert x (v + 1) m)

-- Using the helper functions this creates and sorts the WordCount list
countWords :: String -> [WordCount]
countWords text =
  let
    splitted = words text
  in
    map assocToWordCount $ Map.assocs (countWordsMap splitted  Map.empty)
    
-- Server implementation for WordsCountAPI type
countWordsApi :: Server WordsCountAPI
countWordsApi multipartData sortBy = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)
      
    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content

  return []
  
-- * data

-- This type is to define the order of the word count result
data SortBy = Asc | Desc deriving (Bounded, Show, Enum)

--This instance is necessary for the QueryParam definition as it needs some way to parse the text to the specified type
instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

data WordCount
  = WordCount {
    word :: String,
    count :: Integer
  } deriving (Eq, Show, Generic)

-- Intances necessary for mapping the WordCount type to/from json
instance FromJSON WordCount
instance ToJSON WordCount
