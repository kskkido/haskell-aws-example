module Portfolio.Lib.Contentful.Data.ItemSysContentType.Main
  ( ItemSysContentType(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.ItemSysContentTypeSys.Main as ItemSysContentTypeSys

data ItemSysContentType = ItemSysContentType
  { sys :: ItemSysContentTypeSys.ItemSysContentTypeSys
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ItemSysContentType
fromResponseJSON = Aeson.withObject "itemSysContentType" $ \x -> ItemSysContentType
  <$> Aeson.Types.explicitParseField (ItemSysContentTypeSys.fromResponseJSON) x "sys"
