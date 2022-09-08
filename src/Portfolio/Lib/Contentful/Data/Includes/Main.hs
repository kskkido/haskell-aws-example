module Portfolio.Lib.Contentful.Data.Includes.Main
  ( Includes(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.Asset.Main as Asset

newtype Includes = Includes
  { asset :: Asset.Asset
  }
  deriving (Eq, Show)

unit :: Includes
unit = Includes Asset.unit

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser Includes
fromResponseJSON = Aeson.withObject "includes" $ \x ->
  Control.Monad.msum
    [ ( ( Aeson.Types.explicitParseField Asset.fromResponseJSON x "Asset" ) <&>
        ( Includes )
      )
    , ( pure unit
      )
    ]
