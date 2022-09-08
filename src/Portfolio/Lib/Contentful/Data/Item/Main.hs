module Portfolio.Lib.Contentful.Data.Item.Main
  ( Item(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.ItemSys.Main as ItemSys

data Item = Item
  { fields :: Aeson.Value
  , sys :: ItemSys.ItemSys
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser Item
fromResponseJSON = Aeson.withObject "item" $ \x -> Item
  <$> x Aeson..: "fields"
  <*> Aeson.Types.explicitParseField ItemSys.fromResponseJSON x "sys"
