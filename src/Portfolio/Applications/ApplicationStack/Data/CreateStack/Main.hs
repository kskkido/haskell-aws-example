module Portfolio.Applications.ApplicationStack.Data.CreateStack.Main
  ( fromTemplate
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Monad.Reader as Reader
import qualified Amazonka.CloudFormation.CreateStack
import qualified Amazonka.CloudFormation.Types
import qualified Stratosphere
import qualified Portfolio.Applications.ApplicationStack.Data.App.Main as App
import qualified Portfolio.Applications.ApplicationStack.Data.Template.Main as Template

fromTemplate :: Monad m => Stratosphere.Template -> App.App m Amazonka.CloudFormation.CreateStack.CreateStack
fromTemplate template = do
  config <- Reader.ask
  pure
    ( ( Amazonka.CloudFormation.CreateStack.newCreateStack
        ( Text.pack config.stackName )
      ) &
      ( Amazonka.CloudFormation.CreateStack.createStack_templateBody ?~
        ( Template.toText template )
      ) &
      ( Amazonka.CloudFormation.CreateStack.createStack_capabilities ?~
        ( [Amazonka.CloudFormation.Types.Capability_CAPABILITY_NAMED_IAM] )
      )
    )
