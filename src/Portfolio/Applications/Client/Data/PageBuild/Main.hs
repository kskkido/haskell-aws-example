module Portfolio.Applications.Client.Data.PageBuild.Main
  ( PageBuild(..)
  ) where

import RIO
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata

data PageBuild a = PageBuild
  { metadata :: PageMetadata.PageMetadata
  , content :: a
  }
