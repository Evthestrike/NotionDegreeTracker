{-# LANGUAGE OverloadedStrings #-}

module ParseCourse
  ( parseInput,
  )
where

import Control.Lens
import Course
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

coursesKey :: (AsValue t) => Traversal' t (V.Vector Value)
coursesKey = key "results" . _Array

idKey :: (AsValue t) => Traversal' t T.Text
idKey = key "id" . _String

nameKey :: (AsValue t) => Traversal' t T.Text
nameKey = key "properties" . key "Name" . key "title" . _Array . traversed . key "text" . key "content" . _String

creditsKey :: (AsValue t) => Traversal' t Integer
creditsKey = key "properties" . key "Credits" . key "number" . _Integer

prereqsKey :: (AsValue t) => Traversal' t (V.Vector Value)
prereqsKey = key "properties" . key "Prereq" . key "relation" . _Array

parseCourse :: (AsValue s) => s -> Maybe Course
parseCourse courseJSON =
  Course
    <$> (courseJSON ^? idKey)
    <*> (courseJSON ^? nameKey)
    <*> (courseJSON ^? creditsKey)
    <*> ((courseJSON ^? prereqsKey) >>= traverse (^? idKey))

parseCourses :: V.Vector Value -> V.Vector Course
parseCourses = V.fromList . mapMaybe parseCourse . V.toList

parseInput :: (AsValue a) => Maybe a -> Maybe (V.Vector Course)
parseInput inputJSON = (inputJSON >>= (^? coursesKey)) <&> parseCourses