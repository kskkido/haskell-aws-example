module Portfolio.Applications.DeployRoleStack.Data.Stack.Main
  ( fromTemplate
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Exception.Lens as Exception.Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Except as Except
import qualified Amazonka
import qualified Amazonka.CloudFormation.CreateStack
import qualified Amazonka.CloudFormation.UpdateStack
import qualified Amazonka.CloudFormation.Types
import qualified Stratosphere
import qualified Portfolio.Applications.DeployRoleStack.Data.App.Main as App
import qualified Portfolio.Applications.DeployRoleStack.Data.Template.Main as Template

fromTemplate :: MonadIO m => Stratosphere.Template -> App.App m ()
fromTemplate template = do
  config <- Reader.ask
  void $ liftIO $ Amazonka.runResourceT do
    let createStack =
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
        updateStack =
          ( ( Amazonka.CloudFormation.UpdateStack.newUpdateStack
              ( Text.pack config.stackName )
            ) &
            ( Amazonka.CloudFormation.UpdateStack.updateStack_templateBody ?~
              ( Template.toText template )
            ) &
            ( Amazonka.CloudFormation.UpdateStack.updateStack_capabilities ?~
              ( [Amazonka.CloudFormation.Types.Capability_CAPABILITY_NAMED_IAM] )
            )
          )
    id
      ( ( Amazonka.send config.amazonka createStack $> () ) &
        ( Exception.Lens.handling_ Amazonka.CloudFormation.Types._AlreadyExistsException
            ( Amazonka.send config.amazonka updateStack $> () )
        )
      )
