module Portfolio.Lib.Contentful.Queries.Main
  ( getEntries
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import qualified Portfolio.Lib.Contentful.Data.Config.Main as Config
import qualified Portfolio.Lib.Contentful.Data.Query.Main as Query
import qualified Portfolio.Lib.Contentful.Data.GetEntriesParameters.Main as GetEntriesParameters
import qualified Portfolio.Lib.Contentful.Data.GetEntriesResponse.Main as GetEntriesResponse
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Entries

getEntries :: (Config.HasConfig a) => GetEntriesParameters.GetEntriesParameters -> Query.Query a Entries.Entries
getEntries parameters = do
  config <- Reader.asks Config.get
  req <- HTTP.parseRequest config.baseUrl
       <&> HTTP.setRequestMethod "GET"
       <&> HTTP.setRequestPath (encodeUtf8 $ Text.pack $ Config.toPath config <> "/entries")
       <&> HTTP.addToRequestQueryString (Config.toQuery config <> GetEntriesParameters.toQuery parameters)
  res <- HTTP.httpBS req
  val <- Query.fromEither $ Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
  payload <- Query.fromEither $ Aeson.Types.parseEither GetEntriesResponse.fromResponseJSON val
  pure $ GetEntriesResponse.toEntries payload
