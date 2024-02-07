{-# LANGUAGE OverloadedStrings #-}

module ParseCourse
  ( parseCourseInput,
  )
where

import Control.Arrow
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

yearKey :: (AsValue t) => Traversal' t Year
yearKey = key "properties" . key "Year" . key "select" . key "name" . _JSON

semesterKey :: (AsValue t) => Traversal' t Semester
semesterKey = key "properties" . key "Semester" . key "select" . key "name" . _JSON

parseCourse :: (AsValue t) => t -> Maybe Course
parseCourse courseJSON =
  ( \x ->
      Course
        { idTxt = x,
          name = courseJSON ^? nameKey,
          credits = courseJSON ^? creditsKey,
          prereqs = (courseJSON ^? prereqsKey) >>= traverse (^? idKey),
          year = courseJSON ^? yearKey,
          semester = courseJSON ^? semesterKey
        }
  )
    <$> courseJSON ^? idKey

parseCourses :: V.Vector Value -> V.Vector Course
parseCourses = V.fromList . mapMaybe parseCourse . V.toList

parseCourseInput :: (AsValue a) => Maybe a -> Maybe (V.Vector Course)
parseCourseInput inputJSON = (inputJSON >>= (^? coursesKey)) <&> parseCourses