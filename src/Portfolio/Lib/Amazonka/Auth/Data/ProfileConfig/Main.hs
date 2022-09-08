module Portfolio.Lib.Amazonka.Auth.Data.ProfileConfig.Main
  ( ProfileConfig(..)
  , fromIni
  , fromIniSection
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.HashMap as HashMap
import qualified Amazonka
import qualified Data.Ini as Ini
import qualified Data.Foldable as Foldable
import qualified Portfolio.Lib.Either.Main as Lib.Either

data ProfileConfig = ProfileConfig
  { region :: Amazonka.Region
  , roleArn :: Text
  , sourceProfile :: Text
  }
  deriving (Generic, Eq, Show)

fromIni :: Text -> Ini.Ini -> Either String ProfileConfig
fromIni profileName (Ini.Ini {Ini.iniSections = sections}) =
  ( ( Foldable.asum
      [ HashMap.lookup ("profile " <> profileName) sections
      , HashMap.lookup profileName sections
      ]
    ) &
    ( Lib.Either.fromMaybe "missing section" )
  ) >>= fromIniSection

fromIniSection :: [(Text, Text)] -> Either String ProfileConfig
fromIniSection section =
  ProfileConfig <$>
    ( ( List.lookup "region" section ) &
      ( Lib.Either.fromMaybe "missing region" ) >>=
      ( Amazonka.fromText )
    ) <*>
    ( ( List.lookup "role_arn" section ) &
      ( Lib.Either.fromMaybe "missing source_profile" )
    ) <*>
    ( ( List.lookup "source_profile" section ) &
      ( Lib.Either.fromMaybe "missing source_profile" )
    )
