module Portfolio.Lib.UTCTime.Main
  ( Time.Clock.UTCTime(..)
  , fromIso8601String
  , fromIso8601JSON
  , toDay
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar as Time.Calendar
import qualified Data.Time.Format.ISO8601 as ISO8601

fromIso8601String :: String -> Maybe.Maybe Time.Clock.UTCTime
fromIso8601String = ISO8601.iso8601ParseM

fromIso8601JSON :: Aeson.Value -> Aeson.Types.Parser Time.Clock.UTCTime
fromIso8601JSON = Aeson.withText "utcTime" $ \x -> do
  Maybe.maybe (fail "invalid utc time") pure $ fromIso8601String $ Text.unpack x

toDay :: Time.Clock.UTCTime -> Time.Calendar.Day
toDay = Time.Clock.utctDay
