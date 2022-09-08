module Portfolio.Applications.Client.Data.Image.Main
  ( Image(..)
  ) where

import RIO
import qualified Portfolio.Lib.Lucid.Data.DataAttributes.Main as Lib.Lucid.DataAttributes

data Image = Image
  { src :: String
  , width :: Float
  , height :: Float
  , dataAttributes :: Lib.Lucid.DataAttributes.DataAttributes
  }
  deriving (Eq, Show)
