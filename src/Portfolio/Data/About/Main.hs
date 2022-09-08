module Portfolio.Data.About.Main
  ( About(..)
  , fromFields
  , fromEntries
  ) where

import RIO hiding (link)
import qualified Control.Monad
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Portfolio.Data.AboutFields.Main as AboutFields
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode

data About = About
  { title :: String
  , content :: ContentNode.ContentNode
  }
  deriving (Eq, Show)

fromFields :: AboutFields.AboutFields -> About
fromFields fields = About
  { title = fields.title
  , content = fields.content
  }

fromEntries :: Entries.Entries -> [About]
fromEntries entries =
  ( ( flip Maybe.mapMaybe entries.items $ \item -> do
        Control.Monad.guard (item.sys.contentType.sys.id == "about")
        fields <- Aeson.Types.parseMaybe AboutFields.fromResponseJSON item.fields
        pure $ fromFields fields
    )
  )

