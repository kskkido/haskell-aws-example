module Portfolio.Applications.Client.Data.AppContext.Main
  ( AppContext(..)
  , fromConfig
  , toPageContext
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified System.IO as IO
import qualified Portfolio.Lib.Contentful.Data.Config.Main as Contentful.Config
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Lib.Localize.Data.Source.Main as Localize.Source
import qualified Portfolio.Lib.Localize.Data.SourceByLocale.Main as Localize.SourceByLocale
import qualified Portfolio.Lib.Redis.Data.Config.Main as Redis.Config
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Data.PageContext.Main as PageContext
import qualified Portfolio.Applications.Client.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Repositories.Page.Data.Repository.Main as Page.Repository
import qualified Portfolio.Repositories.Entries.Data.Repository.Main as Entries.Repository

data AppContext = AppContext
  { stage :: Text
  , defaultLocale :: Localize.Locale.Locale
  , pathConfig :: PathConfig.PathConfig
  , localizeConfig :: Localize.Config.Config
  , pageRepository :: Page.Repository.Repository
  , siteMetadata :: SiteMetadata.SiteMetadata
  , entriesRepository :: Entries.Repository.Repository
  }

instance PathConfig.HasPathConfig AppContext where
  get env = env.pathConfig
instance SiteMetadata.HasSiteMetadata AppContext where
  get env = env.siteMetadata
instance Localize.Config.HasConfig AppContext where
  get env = env.localizeConfig
  set cx env = env { localizeConfig = cx }

fromConfig :: AppConfig.AppConfig -> Localize.Locale.Locale -> IO.IO AppContext
fromConfig config locale = do
  sourceByLocale <- do
    en <-
      ( (Localize.Source.fromFile config.filePathTranslationsEn) <&>
        (fmap $ Localize.SourceByLocale.fromSource Localize.Locale.En)
      )
    ja <-
      ( (Localize.Source.fromFile config.filePathTranslationsJa) <&>
        (fmap $ Localize.SourceByLocale.fromSource Localize.Locale.Ja)
      )
    pure $ Foldable.foldMap Foldable.fold [en, ja]
  pure $ AppContext
    { stage = Text.pack config.stage
    , defaultLocale = config.defaultLocale
    , pathConfig = PathConfig.PathConfig
      { PathConfig.delimiter = '/'
      , PathConfig.parameter = ':'
      }
    , localizeConfig = Localize.Config.Config
      { Localize.Config.locale = locale
      , Localize.Config.sourceByLocale = sourceByLocale
      }
    , pageRepository = Page.Repository.fromRedis $ Redis.Config.Config
      { Redis.Config.host = config.redisHost
      , Redis.Config.port = config.redisPort
      , Redis.Config.database = config.redisDatabase
      , Redis.Config.maxConnections = 1
      , Redis.Config.maxIdleTime = 30
      }
    , entriesRepository = Entries.Repository.fromContentful $ Contentful.Config.Config
      { Contentful.Config.spaceId = config.contentfulSpaceId
      , Contentful.Config.accessToken = config.contentfulAccessToken
      , Contentful.Config.environmentId = config.contentfulEnvironmentId
      , Contentful.Config.baseUrl = config.contentfulBaseUrl
      , Contentful.Config.locale = pure $ Localize.Locale.toString locale
      }
    , siteMetadata = SiteMetadata.SiteMetadata
      { SiteMetadata.home = PageMetadata.PageMetadata
        { PageMetadata.title = "pages.home.title"
        , PageMetadata.pathInfo = []
        }
      , SiteMetadata.about = PageMetadata.PageMetadata
        { PageMetadata.title = "pages.about.title"
        , PageMetadata.pathInfo = ["about"]
        }
      , SiteMetadata.photographs = PageMetadata.PageMetadata
        { PageMetadata.title = "pages.photographs.title"
        , PageMetadata.pathInfo = ["photographs"]
        }
      , SiteMetadata.translations = PageMetadata.PageMetadata
        { PageMetadata.title = "pages.translations.title"
        , PageMetadata.pathInfo = ["translations"]
        }
      , SiteMetadata.drawings = PageMetadata.PageMetadata
        { PageMetadata.title = "pages.drawings.title"
        , PageMetadata.pathInfo = ["drawings"]
        }
      }
    }

toPageContext :: PageKey.PageKey -> AppContext -> PageContext.PageContext
toPageContext page env =
  PageContext.PageContext
    { pageMetadata = Reader.runReader (PageKey.toPageMetadata page) env
    , siteMetadata = env.siteMetadata
    , pathConfig = env.pathConfig
    , localizeConfig = env.localizeConfig
    , filePathConfig = FilePathConfig.FilePathConfig
      { FilePathConfig.styles = "/styles/index.css"
      , FilePathConfig.public = ""
      }
    }


