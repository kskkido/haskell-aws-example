module Portfolio.Lib.Localize.Data.SourceByLocale.Main
  ( SourceByLocale(..)
  , fromSource
  , fromJSON
  , fromFile
  , lookup
  ) where

import RIO hiding (lookup)
import qualified System.IO as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Lib.Localize.Data.Source.Main as Source

data SourceByLocale = SourceByLocale
  { en :: Source.Source
  , ja :: Source.Source
  }
  deriving (Eq, Show)

instance Semigroup SourceByLocale where
  mx <> my = SourceByLocale
    { en = mx.en <> my.en
    , ja = mx.ja <> my.ja
    }
instance Monoid SourceByLocale where
  mempty = unit
  mappend mx my = mx <> my

unit :: SourceByLocale
unit = SourceByLocale
  { en = mempty
  , ja = mempty
  }

fromSource :: Locale.Locale -> Source.Source -> SourceByLocale
fromSource lx source = case lx of
  Locale.En -> unit { en = source }
  Locale.Ja -> unit { ja = source }

fromJSON :: Aeson.Value -> Aeson.Types.Parser SourceByLocale
fromJSON = Aeson.withObject "sourceByLocale" $ \x -> SourceByLocale
  <$> Aeson.Types.explicitParseField Source.fromJSON x "en"
  <*> Aeson.Types.explicitParseField Source.fromJSON x "ja"

fromFile :: IO.FilePath -> IO.IO (Aeson.Types.Result SourceByLocale)
fromFile path = do
  mv <- fmap (Aeson.Types.parse fromJSON) . Aeson.decode <$> ByteString.Lazy.Char8.readFile path
  maybe (fail "invalid file") pure mv

lookup :: Locale.Locale -> SourceByLocale -> Source.Source
lookup lx = case lx of
  Locale.En -> en
  Locale.Ja -> ja
