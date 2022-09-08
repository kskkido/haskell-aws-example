module Portfolio.Lib.ByteString.Main
  ( toString
  , toLazy
  , toText
  , fromLazy
  , fromLazyChar8
  , fromUtf8Text
  , fromUtf8String
  , fromFilePath
  , fromUtf8FilePath
  , fromAnyFilePath
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.ByteString.UTF8 as ByteString.UTF8

toLazy :: ByteString.ByteString -> ByteString.Lazy.ByteString
toLazy = ByteString.Lazy.fromStrict

toText :: ByteString.ByteString -> Text.Text
toText = Text.pack . toString

toString :: ByteString.ByteString -> String
toString = fmap (Char.chr . fromEnum) . ByteString.unpack

fromLazy :: ByteString.Lazy.ByteString -> ByteString.ByteString
fromLazy = ByteString.Lazy.toStrict

fromLazyChar8 :: ByteString.Lazy.Char8.ByteString -> ByteString.ByteString
fromLazyChar8 = ByteString.Lazy.Char8.toStrict

fromUtf8Text :: Text -> ByteString.ByteString
fromUtf8Text = fromUtf8String . Text.unpack

fromUtf8String :: String -> ByteString.ByteString
fromUtf8String = ByteString.UTF8.fromString

fromFilePath :: FilePath.FilePath -> IO.IO ByteString.ByteString
fromFilePath filePath = fromLazy <$> ByteString.Lazy.readFile filePath

fromUtf8FilePath :: FilePath.FilePath -> IO.IO ByteString.ByteString
fromUtf8FilePath filePath = fromUtf8String <$> IO.readFile filePath

fromAnyFilePath :: String -> IO.IO ByteString.ByteString
fromAnyFilePath file = Foldable.msum
  [ fromFilePath file
  , fromUtf8FilePath file
  ]

