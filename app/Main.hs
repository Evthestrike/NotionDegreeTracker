{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import Course
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B.ByteString
import Data.Maybe
import qualified Data.Vector as V
import Data.Word as W
import Network.Wreq
import ParseCourse
import System.IO

queryDatabaseURL :: String
queryDatabaseURL = "https://api.notion.com/v1/databases/96d3fb824abe41b1a4d9b9cc74341375/query"

setParagraphURL :: String
setParagraphURL = "https://api.notion.com/v1/blocks/3b1b5186-1778-4b65-83c4-17e8a3faa7bb"

getBearerToken :: IO B.ByteString
getBearerToken = do
  tokenHandle <- openFile "./resources/bearerToken.txt" ReadMode
  contents <- B.hGetContents tokenHandle
  hClose tokenHandle

  let newline :: Word8
      newline = 10
      bearerToken = head . B.split newline $ contents

  return bearerToken

getCourses :: B.ByteString -> IO (V.Vector Course)
getCourses bearerToken = do
  let opts =
        defaults
          & auth
            ?~ oauth2Bearer bearerToken
          & header "Notion-Version"
            .~ ["2022-06-28"]
          & header "Content-Type"
            .~ ["application/json"]

  rsp <- postWith opts queryDatabaseURL ("" :: B.ByteString)

  return $ rsp ^? responseBody & parseInput & fromMaybe V.empty

setParagraphBody :: String -> B.ByteString.LazyByteString
setParagraphBody newText =
  encode $
    object
      [ "paragraph"
          A..= object
            [ "rich_text"
                A..= [ object ["text" A..= object ["content" A..= newText]]
                     ]
            ]
      ]

setParagraph :: B.ByteString -> String -> IO ()
setParagraph bearerToken newText = do
  let opts =
        defaults
          & auth
            ?~ oauth2Bearer bearerToken
          & header "Notion-Version"
            .~ ["2022-06-28"]
          & header "Content-Type"
            .~ ["application/json"]

  _ <- patchWith opts setParagraphURL (setParagraphBody newText)
  return ()

main :: IO ()
main = do
  bearerToken <- getBearerToken
  coursesList <- getCourses bearerToken

  print coursesList
  setParagraph bearerToken . show . sum . fmap (^. credits) $ coursesList