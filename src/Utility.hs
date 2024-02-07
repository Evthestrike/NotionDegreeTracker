{-# LANGUAGE TupleSections #-}

module Utility (sumCredits, creditsPerSemester) where

import Control.Lens
import Course
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V

sumCredits :: (Foldable t, Functor t) => t Course -> Integer
sumCredits = sum . fmap (fromMaybe 0 . credits)

emptyCreditsMap :: M.Map Year (M.Map Semester Integer)
emptyCreditsMap = M.fromList . fmap (,M.fromList . fmap (,0) $ [minBound .. maxBound]) $ [minBound .. maxBound]

creditsPerSemester :: V.Vector Course -> M.Map Year (M.Map Semester Integer)
creditsPerSemester = foldr insertCourse emptyCreditsMap

insertCourse :: Course -> M.Map Year (M.Map Semester Integer) -> M.Map Year (M.Map Semester Integer)
insertCourse (Course {year = Just y, semester = Just s, credits = Just c}) acc = acc & ix y . ix s %~ (+ c)
insertCourse _ acc = acc

-- keyByYear :: [Course] -> M.Map Year [Course]
-- keyByYear = M.fromList . fmap ((,) <$> fromJust . view year . head <*> id) . groupOn (view year)

-- keyBySemester :: [Course] -> M.Map Semester [Course]
-- keyBySemester = M.fromList . fmap ((,) <$> fromJust . view semester . head <*> id) . groupOn (view semester)

-- -- ! fromJust is unsafe
-- creditsPerSemester :: V.Vector Course -> M.Map Year (M.Map Semester Integer)
-- creditsPerSemester = fmap (fmap sumCredits . keyBySemester) . keyByYear . filterInvalid . V.toList
--   where
--     filterInvalid = filter ((\a b c -> a && b && c) <$> isJust . view credits <*> isJust . view semester <*> isJust . view year)

-- creditsPerSemester' :: [Course] -> [Course]
-- creditsPerSemester' courseList = courseList ^.. folded . filtered ((\a b c -> a && b && c) <$> isJust . view credits <*> isJust . view semester <*> isJust . view year)
