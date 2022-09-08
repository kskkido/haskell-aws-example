module Portfolio.Lib.Contentful.Data.ItemFieldsImage.Main
  ( ItemFieldsImage(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.ItemFieldsImageSys.Main as ItemFieldsImageSys

data ItemFieldsImage = ItemFieldsImage
  { sys :: ItemFieldsImageSys.ItemFieldsImageSys
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ItemFieldsImage
fromResponseJSON = Aeson.withObject "itemSysContentTypeSys" $ \x -> ItemFieldsImage
  <$> Aeson.Types.explicitParseField ItemFieldsImageSys.fromResponseJSON x "sys"
