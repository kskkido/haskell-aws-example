module Portfolio.Lib.Amazonka.Auth.Data.CredentialConfig.Main
  ( CredentialConfig(..)
  , fromIni
  , fromIniSection
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.HashMap as HashMap
import qualified Data.Ini as Ini
import qualified Data.Foldable as Foldable
import qualified Amazonka
import qualified Portfolio.Lib.Either.Main as Lib.Either
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString

data CredentialConfig = CredentialConfig
  { region :: Amazonka.Region
  , awsAccessKeyId :: Amazonka.AccessKey
  , awsSecretAccessKey :: Amazonka.Sensitive Amazonka.SecretKey
  }
  deriving (Generic, Eq)

fromIni :: Text -> Ini.Ini -> Either String CredentialConfig
fromIni profileName (Ini.Ini {Ini.iniSections = sections}) =
  ( ( Foldable.asum
      [ HashMap.lookup profileName sections
      ]
    ) &
    ( Lib.Either.fromMaybe "missing section" )
  ) >>= fromIniSection

fromIniSection :: [(Text, Text)] -> Either String CredentialConfig
fromIniSection section =
  CredentialConfig <$>
    ( ( List.lookup "region" section ) &
      ( Lib.Either.fromMaybe "missing region" ) >>=
      ( Amazonka.fromText )
    ) <*>
    ( ( List.lookup "aws_access_key_id" section ) &
      ( Lib.Either.fromMaybe "missing aws_access_key_id" ) >>=
      ( Amazonka.fromText )
    ) <*>
    ( ( List.lookup "aws_secret_access_key" section ) &
      ( Lib.Either.fromMaybe "missing aws_secret_access_key" ) >>=
      ( Amazonka.fromText ) <&>
      ( Amazonka.Sensitive )
    )
