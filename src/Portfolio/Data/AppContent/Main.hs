module Portfolio.Data.AppContent.Main
  ( AppContent(..)
  , fromEntries
  ) where

import RIO
import qualified Portfolio.Data.Photograph.Main as Photograph
import qualified Portfolio.Data.Translation.Main as Translation
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries

data AppContent = AppContent
  { photographs :: [Photograph.Photograph]
  , translations :: [Translation.Translation]
  }
  deriving (Eq, Show)

fromEntries :: Entries.Entries -> AppContent
fromEntries entries = AppContent
  { photographs = Photograph.fromEntries entries
  , translations = Translation.fromEntries entries
  }
