module Portfolio.Lib.Localize.Data.Config.Main
  ( Config(..)
  , HasConfig(..)
  , setLocale
  ) where

import RIO hiding (set)
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Lib.Localize.Data.SourceByLocale.Main as SourceByLocale

data Config = Config
  { locale :: Locale.Locale
  , sourceByLocale :: SourceByLocale.SourceByLocale
  }

class HasConfig a where
  get :: a -> Config
  set :: Config -> a -> a
instance HasConfig Config where
  get = id
  set = const

setLocale :: HasConfig a => Locale.Locale -> a -> a
setLocale lx cx = set ((get cx) { locale = lx }) cx

