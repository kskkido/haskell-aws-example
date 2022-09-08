module Portfolio.Applications.Client.Data.PageMetadata.Main
  ( PageMetadata(..)
  , HasPageMetadata(..)
  , toPath
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Data.Path.Main as Path
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Data.PathConfig.Main as PathConfig

data PageMetadata = PageMetadata
  { title :: String
  , pathInfo :: PathInfo.PathInfo
  }
  deriving (Eq, Show)

class HasPageMetadata a where
  get :: a -> PageMetadata
instance HasPageMetadata PageMetadata where
  get = id

toPath :: (PathConfig.HasPathConfig a, Monad m) => PageMetadata -> Reader.ReaderT a m Path.Path
toPath metadata = PathInfo.toPath metadata.pathInfo
