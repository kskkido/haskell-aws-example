module Portfolio.Lib.Contentful.Data.GetEntriesResponse.Main
  ( GetEntriesResponse(..)
  , fromResponseJSON
  , toEntries
  ) where

import RIO hiding (link)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.Items.Main as Items
import qualified Portfolio.Lib.Contentful.Data.Includes.Main as Includes
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries

data GetEntriesResponse = GetEntriesResponse
  { total :: Int
  , skip :: Int
  , limit :: Int
  , items :: Items.Items
  , includes :: Includes.Includes
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser GetEntriesResponse
fromResponseJSON = Aeson.withObject "entry" $ \x -> GetEntriesResponse
  <$> x Aeson..: "total"
  <*> x Aeson..: "skip"
  <*> x Aeson..: "limit"
  <*> Aeson.Types.explicitParseField Items.fromResponseJSON x "items"
  <*> Aeson.Types.explicitParseField Includes.fromResponseJSON x "includes"

toEntries :: GetEntriesResponse -> Entries.Entries
toEntries x = Entries.Entries
  { Entries.asset = x.includes.asset
  , Entries.items = x.items
  }
