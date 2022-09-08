module Portfolio.Data.TranslationFields.Main
  ( TranslationFields(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Time.Clock

data TranslationFields = TranslationFields
  { title :: String
  , publishedAt :: Time.Clock.UTCTime
  , link :: Maybe.Maybe String
  }
  deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser TranslationFields
fromResponseJSON = Aeson.withObject "translationFields" $ \x -> TranslationFields
  <$> x Aeson..: "title"
  <*> x Aeson..: "publishedAt"
  <*> x Aeson..:? "link"

