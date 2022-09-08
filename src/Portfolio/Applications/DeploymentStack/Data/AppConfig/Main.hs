module Portfolio.Applications.DeploymentStack.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Amazonka
import qualified Amazonka.Env
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Lib.Amazonka.Auth.Main as Lib.Amazonka.Auth

data AppConfig = AppConfig
  { stage :: String
  , stackName :: String
  , appStackName :: String
  , serviceName :: String
  , lambdaBucketName :: String
  , deployRoleAwsAccountId :: String
  , deployRoleAwsUserName :: String
  , staticSiteBucketNamePrefix :: String
  , amazonka :: Amazonka.Env.Env
  } deriving (Generic)

fromSystem :: IO.IO (Either String AppConfig)
fromSystem = do
  httpManager <- Client.newManager Client.Conduit.tlsManagerSettings
  Except.runExceptT do
    stage <- Except.ExceptT do
      Either.fromMaybe "Invalid STAGE" <$> do
        Environment.lookupEnv "STAGE"
    stackName <- Except.ExceptT do
      Either.fromMaybe "Invalid STACK_NAME" <$> do
        Environment.lookupEnv "STACK_NAME"
    appStackName <- Except.ExceptT do
      Either.fromMaybe "Invalid APP_STACK_NAME" <$> do
        Environment.lookupEnv "APP_STACK_NAME"
    serviceName <- Except.ExceptT do
      Either.fromMaybe "Invalid SERVICE_NAME" <$> do
        Environment.lookupEnv "SERVICE_NAME"
    lambdaBucketName <- Except.ExceptT do
      Either.fromMaybe "Invalid LAMBDA_BUCKET_NAME" <$> do
        Environment.lookupEnv "LAMBDA_BUCKET_NAME"
    deployRoleAwsAccountId <- Except.ExceptT do
      Either.fromMaybe "Invalid DEPLOY_ROLE_AWS_ACCOUNT_ID" <$> do
        Environment.lookupEnv "DEPLOY_ROLE_AWS_ACCOUNT_ID"
    deployRoleAwsUserName <- Except.ExceptT do
      Either.fromMaybe "Invalid DEPLOY_ROLE_AWS_USER_NAME" <$> do
        Environment.lookupEnv "DEPLOY_ROLE_AWS_USER_NAME"
    staticSiteBucketNamePrefix <- Except.ExceptT do
      Either.fromMaybe "Invalid STATIC_SITE_BUCKET_NAME_PREFIX" <$> do
        Environment.lookupEnv "STATIC_SITE_BUCKET_NAME_PREFIX"
    amazonka <- liftIO do
      auth <- second (Maybe.fromMaybe Amazonka.Tokyo) <$> Lib.Amazonka.Auth.fromSystem httpManager
      pure $ Lib.Amazonka.Auth.authenticate auth (Amazonka.newEnvWith httpManager)
    return $ AppConfig
      { stage = stage
      , stackName = stackName
      , appStackName = appStackName
      , serviceName = serviceName
      , lambdaBucketName = lambdaBucketName
      , deployRoleAwsAccountId = deployRoleAwsAccountId
      , deployRoleAwsUserName = deployRoleAwsUserName
      , staticSiteBucketNamePrefix = staticSiteBucketNamePrefix
      , amazonka = amazonka
      }

