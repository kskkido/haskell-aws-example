module Portfolio.Applications.Client.Data.PageContext.Main
  ( PageContext(..)
  , HasPageContext(..)
  ) where

import RIO
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig

data PageContext = PageContext
  { pageMetadata :: PageMetadata.PageMetadata
  , siteMetadata :: SiteMetadata.SiteMetadata
  , pathConfig :: PathConfig.PathConfig
  , localizeConfig :: Localize.Config.Config
  , filePathConfig :: FilePathConfig.FilePathConfig
  }

class HasPageContext a where
  get :: a -> PageContext
instance HasPageContext PageContext where
  get = id
instance Localize.Config.HasConfig PageContext where
  get env = env.localizeConfig
  set cx env = env { localizeConfig = cx }
instance PageMetadata.HasPageMetadata PageContext where
  get env = env.pageMetadata
instance SiteMetadata.HasSiteMetadata PageContext where
  get env = env.siteMetadata
instance PathConfig.HasPathConfig PageContext where
  get env = env.pathConfig
instance FilePathConfig.HasFilePathConfig PageContext where
  get env = env.filePathConfig

