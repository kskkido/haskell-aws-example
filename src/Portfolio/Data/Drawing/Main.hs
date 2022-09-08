module Portfolio.Data.Drawing.Main
  ( Drawing(..)
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
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Portfolio.Data.DrawingFields.Main as DrawingFields
import qualified Portfolio.Lib.Contentful.Data.Asset.Main as Asset
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode
import qualified Portfolio.Lib.Day.Main as Day

data Drawing = Drawing
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

fromEntry :: DrawingFields.DrawingFields -> Asset.Asset -> Maybe.Maybe Drawing
fromEntry fields assets = do
  image <- flip List.find assets $ \asset -> asset.sys.id == fields.image.sys.id
  pure $ Drawing
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

fromEntries :: Entries.Entries -> [Drawing]
fromEntries entries =
  ( ( flip Maybe.mapMaybe entries.items $ \item -> do
        Control.Monad.guard (item.sys.contentType.sys.id == "drawing")
        fields <- Aeson.Types.parseMaybe DrawingFields.fromItem item
        fromEntry fields entries.asset
    )
  )

toDataAttributes :: Drawing -> Map.Map Text Text
toDataAttributes drawing = Map.fromList
  [ ("id", Text.pack drawing.id)
  , ("title", Text.pack drawing.title)
  , ("created_at", Text.pack $ show drawing.createdAt)
  , ("url", Text.pack drawing.url)
  , ("filename", Text.pack drawing.url)
  , ("size", Text.pack $ show drawing.size)
  , ("width", Text.pack $ show drawing.width)
  , ("height", Text.pack $ show drawing.height)
  ]

compareCreatedAt :: Drawing -> Drawing -> Ord.Ordering
compareCreatedAt = Ord.compare `on` createdAt

groupByMonth :: [Drawing] -> [(Time.Calendar.Day, [Drawing])]
groupByMonth drawings =
  ( ( drawings ) &
    ( map \drawing ->
        ( ( ( Time.Clock.utctDay drawing.createdAt ) &
            ( Day.toFirstDayOfMonth )
          )
        , [ drawing ]
        )
    ) &
    ( Map.fromListWith (flip (++)) ) &
    ( Map.toList )
  )
