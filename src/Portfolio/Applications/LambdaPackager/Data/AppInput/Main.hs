module Portfolio.Applications.LambdaPackager.Data.AppInput.Main
  ( AppInput(..)
  ) where

import RIO
import qualified System.FilePath as FilePath

data AppInput =
  AppCopyInput
    { input :: FilePath.FilePath
    , output :: FilePath.FilePath
    }
  deriving (Eq, Show, Generic)

