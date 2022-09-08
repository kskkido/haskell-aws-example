module Portfolio.Lib.Contentful.Data.RichText.Data.DocumentNodeType.Main
  ( DocumentNodeType(..)
  , fromResponseJSON
  ) where

import RIO hiding (fromString)
import qualified RIO.Text as Text
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data DocumentNodeType = DocumentNodeType
  deriving (Eq, Show)

instance Aeson.FromJSON DocumentNodeType where
  parseJSON = fromResponseJSON

fromString :: String -> Maybe.Maybe DocumentNodeType
fromString x = do
  Control.Monad.guard (x == "document")
  pure DocumentNodeType

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser DocumentNodeType
fromResponseJSON = Aeson.withText "nodeType" $ \x -> do
  Maybe.maybe (fail "invalid nodetype") pure $ fromString $ Text.unpack x
