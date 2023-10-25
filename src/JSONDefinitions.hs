{-# LANGUAGE OverloadedStrings #-}

module JSONDefinitions (setParagraphBody) where

import Data.Aeson
import Data.ByteString.Lazy (LazyByteString)

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