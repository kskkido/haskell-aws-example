module Portfolio.Applications.DeploymentStack.Data.Template.Main
  ( toText
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Stratosphere
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString

toText :: Stratosphere.Template -> Text.Text
toText template =
  ( ( Stratosphere.encodeTemplate template ) &
    ( Lib.ByteString.fromLazyChar8 ) &
    ( Lib.ByteString.toText )
  )
