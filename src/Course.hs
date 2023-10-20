{-# LANGUAGE TemplateHaskell #-}

module Course
  ( Course (Course),
    idTxt,
    name,
    credits,
    prereqs,
  )
where

import Control.Lens
import Data.Text
import qualified Data.Vector as V

data Course = Course
  { _idTxt :: Text,
    _name :: Text,
    _credits :: Integer,
    _prereqs :: V.Vector Text
  }
  deriving (Show)

$(makeLenses ''Course)