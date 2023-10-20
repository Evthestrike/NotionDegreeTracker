{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Course
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Data.Maybe
import Data.Text
import qualified Data.Vector as V
import Data.Vector.Lens
import Network.Wreq
import ParseCourse

bearerToken :: B.ByteString
bearerToken = ""

queryDatabaseURL :: String
queryDatabaseURL = "https://api.notion.com/v1/databases/96d3fb824abe41b1a4d9b9cc74341375/query"

main :: IO ()
main = do
  let opts =
        defaults
          & auth ?~ oauth2Bearer bearerToken
          & header "Notion-Version" .~ ["2022-06-28"]
          & header "Content-Type" .~ ["application/json"]

  rsp <- postWith opts queryDatabaseURL ("" :: B.ByteString)

  let coursesList = rsp ^? responseBody & parseInput & fromMaybe V.empty

  V.mapM_ print coursesList
