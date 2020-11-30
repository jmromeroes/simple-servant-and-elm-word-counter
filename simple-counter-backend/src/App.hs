{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module App where

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

import Codec.Binary.UTF8.String (decode)

import qualified Data.ByteString.Lazy as LBS

import Domain

-- * data

--This instance is necessary for the QueryParam definition as it needs some way to parse the text to the specified type
instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

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
  
-- Server implementation for WordsCountAPI type
countWordsApi :: Server WordsCountAPI
countWordsApi multipartData sortBy_ = do
  let res1 = forM (inputs multipartData) $ \input -> countWords (show (iValue input)) sortBy_
  
      res2 = forM (files multipartData) $ \file -> do
                 let content = fdPayload file
                 countWords (decode $ LBS.unpack content) sortBy_

  return $ sortArray (concat (res1 ++ res2)) sortBy_
