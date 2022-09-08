module Portfolio.Data.Translation.Main
  ( Translation(..)
  , fromFields
  , fromEntries
  , toVariant
  , comparePublishedAt
  ) where

import RIO hiding (link)
import qualified Control.Monad
import qualified Data.Ord as Ord
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Time.Clock
import qualified Portfolio.Data.TranslationFields.Main as TranslationFields
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries
import qualified Portfolio.Data.TranslationLinked.Main as TranslationLinked
import qualified Portfolio.Data.TranslationStatic.Main as TranslationStatic
import qualified Portfolio.Data.TranslationVariant.Main as TranslationVariant

data Translation = Translation
  { title :: String
  , publishedAt :: Time.Clock.UTCTime
  , link :: Maybe.Maybe String
  }
  deriving (Eq, Show)

fromFields :: TranslationFields.TranslationFields -> Translation
fromFields fields = Translation
  { title = fields.title
  , publishedAt = fields.publishedAt
  , link = fields.link
  }

fromEntries :: Entries.Entries -> [Translation]
fromEntries entries =
  ( ( flip Maybe.mapMaybe entries.items $ \item -> do
        Control.Monad.guard (item.sys.contentType.sys.id == "translation")
        fields <- Aeson.Types.parseMaybe TranslationFields.fromResponseJSON item.fields
        pure $ fromFields fields
    )
  )

toVariant :: Translation -> TranslationVariant.TranslationVariant
toVariant translation =
  case translation.link of
    Just link -> TranslationVariant.Linked $ TranslationLinked.TranslationLinked
      { TranslationLinked.title = translation.title
      , TranslationLinked.publishedAt = translation.publishedAt
      , TranslationLinked.link = link
      }
    Nothing   -> TranslationVariant.Static $ TranslationStatic.TranslationStatic
      { TranslationStatic.title = translation.title
      , TranslationStatic.publishedAt = translation.publishedAt
      }

comparePublishedAt :: Translation -> Translation -> Ord.Ordering
comparePublishedAt = Ord.compare `on` publishedAt

