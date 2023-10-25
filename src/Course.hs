{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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
    emptyCourse,
  )
where

import Control.Lens
import Data.Aeson
import Data.Text
import qualified Data.Vector as V
import GHC.Generics

data Year = Freshman | Sophomore | Junior | Senior deriving (Show, Generic, Eq, Ord)

instance FromJSON Year

instance ToJSON Year

data Semester = Fall | Spring deriving (Show, Generic, Eq, Ord)

instance FromJSON Semester

instance ToJSON Semester

data Course = Course
  { _idTxt :: Text,
    _name :: Maybe Text,
    _credits :: Maybe Integer,
    _prereqs :: Maybe (V.Vector Text),
    _year :: Maybe Year,
    _semester :: Maybe Semester
  }
  deriving (Show)

emptyCourse :: Text -> Course
emptyCourse idTextVal =
  Course
    { _idTxt = idTextVal,
      _name = Nothing,
      _credits = Nothing,
      _prereqs = Nothing,
      _year = Nothing,
      _semester = Nothing
    }

$(makeLenses ''Course)