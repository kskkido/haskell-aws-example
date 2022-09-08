module Portfolio.Lib.Contentful.Data.RichText.Data.TextNodeType.Main
  ( TextNodeType(..)
  , fromResponseJSON
  ) where

import RIO hiding (fromString)
import qualified RIO.Text as Text
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data TextNodeType = TextNodeType
  deriving (Eq, Show)

instance Aeson.FromJSON TextNodeType where
  parseJSON = fromResponseJSON

fromString :: String -> Maybe.Maybe TextNodeType
fromString x = do
  Control.Monad.guard (x == "text")
  pure TextNodeType

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser TextNodeType
fromResponseJSON = Aeson.withText "nodeType" $ \x -> do
  Maybe.maybe (fail "invalid nodetype") pure $ fromString $ Text.unpack x
