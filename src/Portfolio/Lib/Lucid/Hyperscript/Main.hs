module Portfolio.Lib.Lucid.Hyperscript.Main
  ( __
  ) where

import RIO
import qualified Lucid
import qualified Lucid.Base

__ :: Text -> Lucid.Attribute
__ = Lucid.Base.makeAttribute "_"

