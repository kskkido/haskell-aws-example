module Portfolio.Data.Sitemap.Main
  ( Sitemap
  ) where

import qualified RIO.Map as Map
import qualified Portfolio.Data.Path.Main as Path

type Sitemap = Map.Map Path.Path Path.Path

