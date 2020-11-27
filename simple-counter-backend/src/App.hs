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
import System.IO

-- * api

type WordsCountAPI =
  "words" :> "count" :> MultipartForm Mem (MultipartData Mem) :> QueryParam "sortBy" SortBy :> Post '[JSON] [WordCount]
  
wordsApi :: Proxy WordsCountAPI
wordsApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve wordsApi server

server :: Server WordsCountAPI
server = countWords

countWords :: Server WordsCountAPI
countWords multipartData = do
  return []
  
-- * data

data SortBy = Asc | Desc

data WordCount
  = WordCount {
    word :: String,
    count :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON WordCount
instance ToJSON WordCount
