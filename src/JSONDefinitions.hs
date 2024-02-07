{-# LANGUAGE OverloadedStrings #-}

module JSONDefinitions (setParagraphBody) where

import Course
import Data.Aeson
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Map as M

setParagraphBody :: String -> LazyByteString
setParagraphBody newText =
  encode $
    object
      [ "paragraph"
          .= object
            [ "rich_text"
                .= [ object ["text" .= object ["content" .= newText]]
                   ]
            ]
      ]

-- showCredits :: M.Map Year (M.Map Semester Integer) -> LazyByteString
-- showCredits = M.foldrWithKey (\y xs acc -> acc ++ M.foldrWithKey (\s x acc2 -> acc2 ++ show s ++ " " ++ show y ++ ": " ++ show x ++ "\n") "" xs) ""