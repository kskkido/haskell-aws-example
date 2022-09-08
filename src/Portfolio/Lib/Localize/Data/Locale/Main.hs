module Portfolio.Lib.Localize.Data.Locale.Main
  ( Locale(..)
  , HasLocale(..)
  , fromString
  , toString
  , toggle
  ) where

import RIO hiding (fromString)
import qualified Control.Monad
import qualified Data.Maybe as Maybe

data Locale =
    En
  | Ja
  deriving (Ord, Eq)

instance Show Locale where
  show = toString
class HasLocale a where
  get :: a -> Locale
instance HasLocale Locale where
  get = id

fromString :: String -> Maybe.Maybe Locale
fromString xs =
  Control.Monad.msum
    [ Control.Monad.guard (xs == "en") $> En
    , Control.Monad.guard (xs == "ja") $> Ja
    ]

toString :: Locale -> String
toString En = "en"
toString Ja = "ja"

toggle :: Locale -> Locale
toggle En = Ja
toggle Ja = En
