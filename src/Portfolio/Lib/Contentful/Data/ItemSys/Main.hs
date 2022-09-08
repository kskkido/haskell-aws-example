module Portfolio.Lib.Contentful.Data.ItemSys.Main
  ( ItemSys(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.ItemSysContentType.Main as ItemSysContentType

data ItemSys = ItemSys
  { contentType :: ItemSysContentType.ItemSysContentType
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ItemSys
fromResponseJSON = Aeson.withObject "itemSys" $ \x -> ItemSys
  <$> Aeson.Types.explicitParseField ItemSysContentType.fromResponseJSON x "contentType"
