{-# LANGUAGE OverloadedStrings #-}

module ParseTableInput (parseIDs) where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.Text as T

idKeys :: (AsValue t) => Traversal' t T.Text
idKeys = key "results" . _Array . folded . key "id" . _String