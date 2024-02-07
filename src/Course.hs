{-# LANGUAGE DeriveGeneric #-}

module Course
  ( Year,
    Semester,
    Course (Course),
    idTxt,
    name,
    credits,
    prereqs,
    year,
    semester,
  )
where

import Data.Aeson
import Data.Text
import qualified Data.Vector as V
import GHC.Generics

data Year = Freshman | Sophomore | Junior | Senior deriving (Show, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON Year

instance ToJSON Year

data Semester = Fall | Spring deriving (Show, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON Semester

instance ToJSON Semester

data Course = Course
  { idTxt :: Text,
    name :: Maybe Text,
    credits :: Maybe Integer,
    prereqs :: Maybe (V.Vector Text),
    year :: Maybe Year,
    semester :: Maybe Semester
  }
  deriving (Show)