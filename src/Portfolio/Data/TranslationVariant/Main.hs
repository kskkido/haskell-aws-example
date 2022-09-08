module Portfolio.Data.TranslationVariant.Main
  ( TranslationVariant(..)
  ) where

import qualified Portfolio.Data.TranslationLinked.Main as TranslationLinked
import qualified Portfolio.Data.TranslationStatic.Main as TranslationStatic

data TranslationVariant =
    Linked TranslationLinked.TranslationLinked
  | Static TranslationStatic.TranslationStatic

