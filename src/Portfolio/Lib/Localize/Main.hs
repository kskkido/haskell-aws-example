module Portfolio.Lib.Localize.Main
  ( translate
  , translateByConfig
  ) where

import RIO
import qualified RIO.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Lib.Localize.Data.Config.Main as Config
import qualified Portfolio.Lib.Localize.Data.SourceByLocale.Main as SourceByLocale

translate :: (Config.HasConfig a, Monad m) => String -> Locale.Locale -> Reader.ReaderT a m String
translate key locale = do
  config <- Config.get <$> Reader.ask
  lift $ pure $ Maybe.fromMaybe key $ do
    let source = SourceByLocale.lookup locale config.sourceByLocale
    Map.lookup key source

translateByConfig :: (Config.HasConfig a, Monad m) => String -> Reader.ReaderT a m String
translateByConfig key = do
  config <- Config.get <$> Reader.ask
  translate key config.locale
