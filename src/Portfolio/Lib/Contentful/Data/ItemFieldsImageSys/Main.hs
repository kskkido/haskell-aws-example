module Portfolio.Lib.Contentful.Data.ItemFieldsImageSys.Main
  ( ItemFieldsImageSys(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data ItemFieldsImageSys = ItemFieldsImageSys
  { id :: String
  , linkType :: String
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ItemFieldsImageSys
fromResponseJSON = Aeson.withObject "itemSysContentTypeSys" $ \x -> ItemFieldsImageSys
  <$> x Aeson..: "id"
  <*> x Aeson..: "linkType"

