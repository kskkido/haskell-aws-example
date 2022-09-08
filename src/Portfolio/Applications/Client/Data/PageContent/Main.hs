module Portfolio.Applications.Client.Data.PageContent.Main
  ( PageContent(..)
  , unit
  , fromEntries
  ) where

import RIO
import qualified Portfolio.Data.About.Main as About
import qualified Portfolio.Data.Drawing.Main as Drawing
import qualified Portfolio.Data.Photograph.Main as Photograph
import qualified Portfolio.Data.Translation.Main as Translation
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries

data PageContent = PageContent
  { about :: [About.About]
  , drawings :: [Drawing.Drawing]
  , photographs :: [Photograph.Photograph]
  , translations :: [Translation.Translation]
  }
  deriving (Eq, Show)

unit :: PageContent
unit = PageContent
  { about = []
  , drawings = []
  , photographs = []
  , translations = []
  }

fromEntries :: Entries.Entries -> PageContent
fromEntries entries = PageContent
  { about = About.fromEntries entries
  , drawings = Drawing.fromEntries entries
  , photographs = Photograph.fromEntries entries
  , translations = Translation.fromEntries entries
  }
