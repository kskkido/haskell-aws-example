module Portfolio.Data.TranslationLinked.Main
  ( TranslationLinked(..)
  ) where

import RIO
import qualified Data.Time.Clock as Time.Clock

data TranslationLinked = TranslationLinked
  { title :: String
  , publishedAt :: Time.Clock.UTCTime
  , link :: String
  }
  deriving (Eq, Show)

