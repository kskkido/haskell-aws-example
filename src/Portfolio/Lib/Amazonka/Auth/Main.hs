module Portfolio.Lib.Amazonka.Auth.Main
  ( fromSystem
  , authenticate
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Data.Ini as Ini
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Amazonka
import qualified Amazonka.Env
import qualified Amazonka.Auth
import qualified Amazonka.STS.AssumeRole
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Portfolio.Lib.Either.Main as Lib.Either
import qualified Portfolio.Lib.Amazonka.Auth.Data.ProfileConfig.Main as ProfileConfig
import qualified Portfolio.Lib.Amazonka.Auth.Data.CredentialConfig.Main as CredentialConfig

fromSystem :: (MonadIO m, Alternative m) => Client.Manager -> m (Amazonka.Auth.Auth, Maybe Amazonka.Region)
fromSystem manager = liftIO do
  let append ma mb = mb `Exception.catch` (\(_ :: Exception.SomeException) -> ma)
  Foldable.foldr
    append
    ( fail "" )
    [ Amazonka.Auth.fromEnv
    , Amazonka.Auth.fromProfile manager
    , do
        profileName <- liftIO do
          menv <- Environment.lookupEnv "AWS_PROFILE"
          maybe (fail "undefined AWS_PROFILE") pure menv
        second pure <$> fromProfile (Text.pack profileName) manager
    ]

fromProfile :: (MonadIO m, Alternative m) => Text -> Client.Manager -> m (Amazonka.Auth.Auth, Amazonka.Region)
fromProfile profileName manager = liftIO do
  Lib.Either.liftFail $ Except.runExceptT do
    credentials <- Except.ExceptT $ liftIO do
      filePath <- Amazonka.Auth.credFile
      Ini.readIniFile filePath
    profiles <- Except.ExceptT $ liftIO do
      filePath <- Amazonka.Auth.confFile
      Ini.readIniFile filePath
    profileConfig <- Except.ExceptT $ pure do
      ProfileConfig.fromIni profileName profiles
    sourceCredentialConfig <- Except.ExceptT $ pure do
      CredentialConfig.fromIni profileConfig.sourceProfile credentials
    let sourceAuth = fromCredentialConfig sourceCredentialConfig
        env = authenticate sourceAuth (Amazonka.newEnvWith manager)
    fromStsRole profileConfig env

fromCredentialConfig :: CredentialConfig.CredentialConfig -> (Amazonka.Auth.Auth, Amazonka.Region)
fromCredentialConfig credentialConfig =
  ( ( Amazonka.Auth.Auth $ Amazonka.AuthEnv
      { Amazonka._authAccessKeyId = credentialConfig.awsAccessKeyId
      , Amazonka._authSecretAccessKey = credentialConfig.awsSecretAccessKey
      , Amazonka._authSessionToken = Nothing
      , Amazonka._authExpiration = Nothing
      }
    )
  , credentialConfig.region
  )

fromStsRole :: (MonadIO m, Alternative m) => ProfileConfig.ProfileConfig -> Amazonka.Env.Env -> m (Amazonka.Auth.Auth, Amazonka.Region)
fromStsRole profileConfig env = liftIO do
  Amazonka.runResourceT do
    let assumeRole =
          ( ( Amazonka.STS.AssumeRole.newAssumeRole
              ( profileConfig.roleArn )
              ( "sts-role" )
            )
          )
    authEnv <- do
      response <- Amazonka.send env assumeRole
      maybe (fail "undefined credentials") pure do
        response ^. Amazonka.STS.AssumeRole.assumeRoleResponse_credentials
    pure
      ( Amazonka.Auth.Auth authEnv
      , Amazonka.Auth._envRegion env
      )

authenticate :: (Amazonka.Auth.Auth, Amazonka.Region) -> Amazonka.Env.EnvNoAuth -> Amazonka.Env.Env
authenticate (auth, region) env = env
  { Amazonka.Auth._envRegion = region
  , Amazonka.Auth._envAuth = Identity auth
  }

