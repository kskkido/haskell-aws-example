module Portfolio.Lib.Contentful.Data.RichText.Data.ParagraphNodeType.Main
  ( ParagraphNodeType(..)
  , fromResponseJSON
  ) where

import RIO hiding (fromString)
import qualified RIO.Text as Text
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data ParagraphNodeType = ParagraphNodeType
  deriving (Eq, Show)

instance Aeson.FromJSON ParagraphNodeType where
  parseJSON = fromResponseJSON

fromString :: String -> Maybe.Maybe ParagraphNodeType
fromString x = do
  Control.Monad.guard (x == "paragraph")
  pure ParagraphNodeType

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser ParagraphNodeType
fromResponseJSON = Aeson.withText "nodeType" $ \x -> do
  Maybe.maybe (fail "invalid nodetype") pure $ fromString $ Text.unpack x
