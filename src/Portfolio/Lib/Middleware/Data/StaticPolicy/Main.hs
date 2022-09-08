module Portfolio.Lib.Middleware.Data.StaticPolicy.Main
  ( fromRedirectPath
  , fromRedirectPaths
  , unit
  ) where

import RIO hiding (to)
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified Control.Monad
import qualified Data.List.Split as List.Split
import qualified Data.Foldable as Foldable
import qualified Network.Wai.Middleware.Static as Static

fromRedirectPath :: IO.FilePath -> IO.FilePath -> Static.Policy
fromRedirectPath from to =
  fold
    [ Static.policy $ \path -> do
        Control.Monad.msum
          [ do
              let fromPathInfo = FilePath.splitPath from & filter (/= "/")
                  pathInfo = FilePath.splitPath path & filter (/= "/")
              Control.Monad.guard (pathInfo == fromPathInfo)
              pure to
          ]
    ]

fromRedirectPaths :: [(IO.FilePath, IO.FilePath)] -> Static.Policy
fromRedirectPaths = Foldable.foldl (Static.<|>) unit . fmap (uncurry fromRedirectPath)

unit :: Static.Policy
unit = Static.policy $ const Nothing
