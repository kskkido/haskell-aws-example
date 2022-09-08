module Portfolio.Lib.Middleware.Main
  ( redirectPath
  , logPath
  , pipe
  ) where

import RIO hiding (to)
import qualified RIO.Text as Text
import qualified Network.Wai
import qualified System.IO as IO
import qualified Control.Monad
import qualified Control.Monad.Extra as Extra
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.List.Split as List.Split

redirectPath :: IO.FilePath -> IO.FilePath -> Network.Wai.Application -> Network.Wai.Application
redirectPath from to callback request respond = do
  let fromPathInfo = Text.pack <$> List.Split.splitOn "/" from
      toPathInfo = Text.pack <$> List.Split.splitOn "/" to
  next <- Extra.fromMaybeM (pure request) $ MaybeT.runMaybeT do
    let pathInfo = Network.Wai.pathInfo request
    liftIO $ IO.print $ "FROM: " <> show fromPathInfo
    liftIO $ IO.print $ "PATH: " <> show pathInfo
    liftIO $ IO.print $ "METHOD: " <> Network.Wai.requestMethod request
    Control.Monad.guard (Network.Wai.requestMethod request == "GET")
    Control.Monad.guard (pathInfo == fromPathInfo)
    liftIO $ IO.print $ "TO: " <> show toPathInfo
    pure request { Network.Wai.pathInfo = toPathInfo }
  callback next respond

logPath :: Network.Wai.Application -> Network.Wai.Application
logPath callback request respond = do
  let pathInfo = Network.Wai.pathInfo request
  liftIO $ IO.print $ "PATH_INFO: " <> show pathInfo
  callback request respond

pipe :: Network.Wai.Middleware -> Network.Wai.Middleware -> Network.Wai.Middleware
pipe mx my callback request response = my (mx callback) request response
