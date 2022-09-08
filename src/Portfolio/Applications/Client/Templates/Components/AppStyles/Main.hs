module Portfolio.Applications.Client.Templates.Components.AppStyles.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Lucid
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig

render :: (FilePathConfig.HasFilePathConfig a, Monad m) => Lucid.HtmlT (Reader.ReaderT a m) ()
render = do
  filePathConfig <- lift $ Reader.asks FilePathConfig.get
  do
    Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ $ Text.pack filePathConfig.styles]
