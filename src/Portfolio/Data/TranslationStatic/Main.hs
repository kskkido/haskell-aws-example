module Portfolio.Data.TranslationStatic.Main
  ( TranslationStatic(..)
  ) where

import RIO
import qualified Data.Time.Clock as Time.Clock

data TranslationStatic = TranslationStatic
  { title :: String
  , publishedAt :: Time.Clock.UTCTime
  }
  deriving (Eq, Show)

