{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Vector as V
import Data.Word as W
import Network.Wreq
import ParseCourse
import System.IO

queryDatabaseURL :: String
queryDatabaseURL = "https://api.notion.com/v1/databases/96d3fb824abe41b1a4d9b9cc74341375/query"

getBearerToken :: IO B.ByteString
getBearerToken = do
  tokenHandle <- openFile "./resources/bearerToken.txt" ReadMode
  contents <- B.hGetContents tokenHandle
  hClose tokenHandle

  let newline :: Word8
      newline = 10
      bearerToken = head . B.split newline $ contents

  return bearerToken

main :: IO ()
main = do
  bearerToken <- getBearerToken

  let opts =
        defaults
          & auth ?~ oauth2Bearer bearerToken
          & header "Notion-Version" .~ ["2022-06-28"]
          & header "Content-Type" .~ ["application/json"]

  rsp <- postWith opts queryDatabaseURL ("" :: B.ByteString)

  let coursesList = rsp ^? responseBody & parseInput & fromMaybe V.empty

  V.mapM_ print coursesList