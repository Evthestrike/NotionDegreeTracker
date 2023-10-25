module Utility (sumCredits, creditsPerSemester) where

import Control.Lens
import Course
import Data.List.Extra (groupOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V

sumCredits :: (Foldable t, Functor t) => t Course -> Integer
sumCredits = sum . fmap (fromMaybe 0 . view credits)

-- ! fromJust is unsafe
creditsPerSemester :: V.Vector Course -> M.Map Year (M.Map Semester Integer)
creditsPerSemester = fmap (fmap sumCredits . keyBySemester) . keyByYear . filterInvalid . V.toList
  where
    filterInvalid = filter ((\a b c -> a && b && c) <$> isJust . view credits <*> isJust . view semester <*> isJust . view year)
    keyByYear = M.fromList . fmap ((,) <$> fromJust . view year . head <*> id) . groupOn (view year)
    keyBySemester = M.fromList . fmap ((,) <$> fromJust . view semester . head <*> id) . groupOn (view semester)