module Portfolio.Services.Contentful.Data.Service.Main
  ( Service(..)
  , fromContentful
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.Contentful.Data.Config.Main as Contentful.Config
import qualified Portfolio.Lib.Contentful.Data.Query.Main as Contentful.Query
import qualified Portfolio.Lib.Contentful.Data.Entries.Main as Contentful.Entries
import qualified Portfolio.Lib.Contentful.Data.GetEntriesParameters.Main as Contentful.GetEntriesParameters
import qualified Portfolio.Lib.Contentful.Queries.Main as Contentful.Queries

data Service = Service
  { getEntries :: IO.IO (Either String Contentful.Entries.Entries)
  }

fromContentful :: Contentful.Config.HasConfig a => a -> Service
fromContentful config = Service
  { getEntries = Contentful.Query.run
      ( ( Contentful.GetEntriesParameters.unit ) &
        ( Contentful.Queries.getEntries )
      )
      config
  }
