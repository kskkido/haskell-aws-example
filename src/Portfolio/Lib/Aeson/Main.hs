module Portfolio.Lib.Aeson.Main
  ( toObject
  , toSerializedText
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding

toObject :: Aeson.Value -> Maybe Aeson.Object
toObject (Aeson.Object x) = Just x
toObject _ = Nothing

toSerializedText :: Aeson.ToJSON a => a -> Text.Text
toSerializedText = Text.Lazy.toStrict . Text.Lazy.Encoding.decodeUtf8 . Aeson.encode
