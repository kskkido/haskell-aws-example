module Portfolio.Applications.Client.Templates.Components.AppScripts.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Lucid

render :: Monad m => Lucid.HtmlT (Reader.ReaderT a m) ()
render = do
  Lucid.script_ [Lucid.src_ "https://kit.fontawesome.com/56e0eb8f60.js", Lucid.crossorigin_ "anonymous"] (Text.pack "")

