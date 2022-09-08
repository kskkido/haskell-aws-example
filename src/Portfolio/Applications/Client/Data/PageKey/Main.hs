module Portfolio.Applications.Client.Data.PageKey.Main
  ( PageKey(..)
  , toPathInfo
  , toPageMetadata
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata

data PageKey =
    Home
  | About
  | Photographs
  | Translations
  | Drawings
  deriving (Eq, Show)

toPathInfo :: (SiteMetadata.HasSiteMetadata a, Monad m) => PageKey -> Reader.ReaderT a m PathInfo.PathInfo
toPathInfo rx = PageMetadata.pathInfo <$> toPageMetadata rx

toPageMetadata :: (SiteMetadata.HasSiteMetadata a, Monad m) => PageKey -> Reader.ReaderT a m PageMetadata.PageMetadata
toPageMetadata rx = do
  siteMetadata <- Reader.asks SiteMetadata.get
  pure $ case rx of
    Home -> siteMetadata.home
    About -> siteMetadata.about
    Photographs -> siteMetadata.photographs
    Translations -> siteMetadata.translations
    Drawings -> siteMetadata.drawings
