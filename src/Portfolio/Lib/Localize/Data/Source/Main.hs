module Portfolio.Lib.Localize.Data.Source.Main
  ( Source
  , fromJSON
  , fromFile
  ) where

import RIO
import qualified RIO.Map as Map
import qualified System.IO as IO
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8

type Source = Map.Map String String

fromJSON :: Aeson.Value -> Aeson.Types.Parser Source
fromJSON = Aeson.withObject "source" $ \x -> Maybe.maybe (fail "invalid source") pure $ do
  txs <- Traversable.for (Aeson.KeyMap.toList x) $ \(key, val) -> do
    rx <- Foldable.fold $ Aeson.fromJSON val
    pure $ (Aeson.Key.toString key, rx)
  pure $ Map.fromList txs

fromFile :: IO.FilePath -> IO.IO (Aeson.Types.Result Source)
fromFile path = do
  mv <- fmap (Aeson.Types.parse fromJSON) . Aeson.decode <$> ByteString.Lazy.Char8.readFile path
  IO.print mv
  maybe (fail "invalid file") pure mv

