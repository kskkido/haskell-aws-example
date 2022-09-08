module Portfolio.Lib.Contentful.Data.Asset.Main
  ( Asset
  , unit
  , fromResponseJSON
  ) where

import RIO
import qualified RIO.Vector as Vector
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.AssetImage.Main as AssetImage

type Asset = [AssetImage.AssetImage]

unit :: Asset
unit = []

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser Asset
fromResponseJSON = Aeson.withArray "asset" $ \xs -> pure
  ( (Vector.mapMaybe (Aeson.Types.parseMaybe AssetImage.fromResponseJSON) xs) &
    (Vector.toList)
  )
