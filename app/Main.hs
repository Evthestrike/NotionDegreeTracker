{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Data.Maybe
import Data.Text
import qualified Data.Vector as V
import Data.Vector.Lens
import Network.Wreq

bearerToken :: B.ByteString
bearerToken = "secret_NycHnxCugzZwLuCqRAxP2RVe7mB7Rxvc0Ims0A1OkJS"

queryDatabaseURL :: String
queryDatabaseURL = "https://api.notion.com/v1/databases/96d3fb824abe41b1a4d9b9cc74341375/query"

courses :: (AsValue t) => Traversal' t (V.Vector Value)
courses = key "results" . _Array

properties :: (AsValue t) => Traversal' t Value
properties = key "properties"

idKey :: (AsValue t) => Traversal' t Text
idKey = key "id" . _String

nameKey :: (AsValue t) => Traversal' t Text
nameKey = key "Name" . key "title" . _Array . traversed . key "text" . key "content" . _String

creditsKey :: (AsValue t) => Traversal' t Integer
creditsKey = key "Credits" . key "number" . _Integer

prereqsKey :: (AsValue t) => Traversal' t (V.Vector Value)
prereqsKey = key "Prereq" . key "relation" . _Array

data Course = Course
  { _id :: Text,
    _name :: Text,
    _credits :: Integer,
    _prereqs :: [Text]
  }
  deriving (Show)

$(makeLenses ''Course)

main :: IO ()
main = do
  let opts =
        defaults
          & auth ?~ oauth2Bearer bearerToken
          & header "Notion-Version" .~ ["2022-06-28"]
          & header "Content-Type" .~ ["application/json"]

  rsp <- postWith opts queryDatabaseURL ("" :: B.ByteString)

  let maybeCoursesJSON = rsp ^? responseBody . courses
      maybeCourses = do
        _id <- maybeCoursesJSON >>= traverse (^? idKey)
        _name <- maybeCoursesJSON >>= traverse (^? properties . nameKey)
        _credits <- maybeCoursesJSON >>= traverse (^? properties . creditsKey)
        _prereqs <- (maybeCoursesJSON >>= traverse (^? properties . prereqsKey)) <&> fmap (fmap (^. key "id" . _String) >>> review vector)
        return (V.zipWith4 Course _id _name _credits _prereqs)
      coursesList = fromMaybe V.empty maybeCourses

  V.mapM_ print coursesList
