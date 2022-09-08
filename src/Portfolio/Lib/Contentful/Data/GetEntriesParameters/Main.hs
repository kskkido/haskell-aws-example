module Portfolio.Lib.Contentful.Data.GetEntriesParameters.Main
  ( GetEntriesParameters(..)
  , unit
  , toQuery
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Types.URI as URI


data GetEntriesParameters = GetEntriesParameters
  { locale :: Maybe.Maybe String
  } deriving (Eq, Show)

unit :: GetEntriesParameters
unit = GetEntriesParameters
  { locale = Maybe.Nothing
  }

toQuery :: GetEntriesParameters -> URI.Query
toQuery params =
    [ params.locale <&> \x -> ("locale", Text.pack x)
    ]
  & Maybe.catMaybes
  & fmap (second $ return . encodeUtf8)

