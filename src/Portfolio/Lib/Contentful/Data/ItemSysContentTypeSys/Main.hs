module Portfolio.Lib.Contentful.Data.ItemSysContentTypeSys.Main
  ( ItemSysContentTypeSys(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data ItemSysContentTypeSys = ItemSysContentTypeSys
  { id :: String
  , linkType :: String
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ItemSysContentTypeSys
fromResponseJSON = Aeson.withObject "itemSysContentTypeSys" $ \x -> ItemSysContentTypeSys
  <$> x Aeson..: "id"
  <*> x Aeson..: "linkType"

