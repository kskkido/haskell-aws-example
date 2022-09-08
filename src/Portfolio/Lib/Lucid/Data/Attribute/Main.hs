module Portfolio.Lib.Lucid.Data.Attribute.Main
  ( fromDataAttributes
  , concat
  ) where

import RIO hiding (concat)
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Base
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Portfolio.Lib.Maybe.Main as Maybe
import qualified Portfolio.Lib.Lucid.Data.DataAttributes.Main as DataAttributes

fromDataAttributes :: DataAttributes.DataAttributes -> [Lucid.Attribute]
fromDataAttributes dataAttributes = Map.toList dataAttributes <&> uncurry Lucid.data_

toClass :: Lucid.Attribute -> Maybe.Maybe Text.Text
toClass (Lucid.Base.Attribute key val) = do
  Control.Monad.guard (key == "class")
  pure val

concat :: [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute]
concat xs ys = concatClasses (xs <> ys)

concatClasses :: [Lucid.Attribute] -> [Lucid.Attribute]
concatClasses xs =
  let (cs,rs) = Maybe.partitionMaybe toClass xs
   in rs <> [Lucid.classes_ $ cs >>= Text.words]

