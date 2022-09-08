module Portfolio.Data.Photograph.Main
  ( Photograph(..)
  , fromEntry
  , fromEntries
  , toDataAttributes
  , compareCreatedAt
  , groupByMonth
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Control.Monad
import qualified Data.Ord as Ord
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar as Time.Calendar
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Portfolio.Data.PhotographFields.Main as PhotographFields
import qualified Portfolio.Lib.Contentful.Data.Asset.Main as Asset
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode
import qualified Portfolio.Lib.Day.Main as Day

data Photograph = Photograph
  { id :: String
  , title :: String
  , createdAt :: Time.Clock.UTCTime
  , url :: String
  , fileName :: String
  , size :: Float
  , width :: Float
  , height :: Float
  , caption :: Maybe.Maybe ContentNode.ContentNode
  }
  deriving (Eq, Show)

fromEntry :: PhotographFields.PhotographFields -> Asset.Asset -> Maybe.Maybe Photograph
fromEntry fields assets = do
  image <- flip List.find assets $ \asset -> asset.sys.id == fields.image.sys.id
  pure $ Photograph
    { id = fields.image.sys.id
    , title = fields.title
    , createdAt = fields.createdAt
    , url = image.fields.file.url
    , fileName = image.fields.file.fileName
    , size = image.fields.file.details.size
    , width = image.fields.file.details.image.width
    , height = image.fields.file.details.image.height
    , caption = fields.caption
    }

fromEntries :: Entries.Entries -> [Photograph]
fromEntries entries =
  ( ( flip Maybe.mapMaybe entries.items $ \item -> do
        Control.Monad.guard (item.sys.contentType.sys.id == "photograph")
        fields <- Aeson.Types.parseMaybe PhotographFields.fromItem item
        fromEntry fields entries.asset
    )
  )

toDataAttributes :: Photograph -> Map.Map Text Text
toDataAttributes photograph = Map.fromList
  [ ("id", Text.pack photograph.id)
  , ("title", Text.pack photograph.title)
  , ("created_at", Text.pack $ show photograph.createdAt)
  , ("url", Text.pack photograph.url)
  , ("filename", Text.pack photograph.url)
  , ("size", Text.pack $ show photograph.size)
  , ("width", Text.pack $ show photograph.width)
  , ("height", Text.pack $ show photograph.height)
  ]

compareCreatedAt :: Photograph -> Photograph -> Ord.Ordering
compareCreatedAt = Ord.compare `on` createdAt

groupByMonth :: [Photograph] -> [(Time.Calendar.Day, [Photograph])]
groupByMonth photographs =
  ( ( photographs ) &
    ( map \photograph ->
        ( ( ( Time.Clock.utctDay photograph.createdAt ) &
            ( Day.toFirstDayOfMonth )
          )
        , [ photograph ]
        )
    ) &
    ( Map.fromListWith (flip (++)) ) &
    ( Map.toList )
  )
