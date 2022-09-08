module Portfolio.Applications.Client.Data.SiteMetadata.Main
  ( SiteMetadata(..)
  , HasSiteMetadata(..)
  , toPaths
  , toMap
  ) where

import RIO
import qualified RIO.Map as Map
import qualified Data.Traversable as Traversable
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Data.Path.Main as Path
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata

data SiteMetadata = SiteMetadata
  { home :: PageMetadata.PageMetadata
  , about :: PageMetadata.PageMetadata
  , translations :: PageMetadata.PageMetadata
  , photographs :: PageMetadata.PageMetadata
  , drawings :: PageMetadata.PageMetadata
  }
  deriving (Eq, Show)

class HasSiteMetadata a where
  get :: a -> SiteMetadata
instance HasSiteMetadata SiteMetadata where
  get = id

toPaths :: (PathConfig.HasPathConfig a, Monad m) => SiteMetadata -> Reader.ReaderT a m [Path.Path]
toPaths siteMetadata = Traversable.sequence
  [ PageMetadata.toPath siteMetadata.home
  , PageMetadata.toPath siteMetadata.about
  , PageMetadata.toPath siteMetadata.translations
  , PageMetadata.toPath siteMetadata.photographs
  , PageMetadata.toPath siteMetadata.drawings
  ]

toMap :: SiteMetadata -> Map.Map String PageMetadata.PageMetadata
toMap siteMetadata = Map.fromList
  [ ("home", siteMetadata.home)
  , ("about", siteMetadata.about)
  , ("translations", siteMetadata.translations)
  , ("photographs", siteMetadata.photographs)
  , ("drawings", siteMetadata.drawings)
  ]
