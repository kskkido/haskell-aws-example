module Portfolio.Lib.ExitCode.Main
  ( liftFail
  ) where

import RIO
import qualified System.Process.Typed as Process
import qualified Control.Monad.Fail as Monad.Fail

liftFail :: Monad.Fail.MonadFail m => m Process.ExitCode -> m ()
liftFail mexitCode = do
  exitCode <- mexitCode
  case exitCode of
    Process.ExitSuccess -> pure ()
    Process.ExitFailure code -> Monad.Fail.fail $ "received exit code of " <> show code
