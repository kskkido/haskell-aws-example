module Portfolio.Lib.Contentful.Data.Items.Main
  ( Items
  , fromResponseJSON
  ) where

import RIO
import qualified RIO.Vector as Vector
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.Item.Main as Item

type Items = [Item.Item]

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser Items
fromResponseJSON = Aeson.withArray "items" $ \xs -> pure
  ( (Vector.mapMaybe (Aeson.Types.parseMaybe Item.fromResponseJSON) xs) &
    (Vector.toList)
  )
