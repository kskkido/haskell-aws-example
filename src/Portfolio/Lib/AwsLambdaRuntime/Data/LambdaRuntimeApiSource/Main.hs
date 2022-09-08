module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeApiSource.Main
  ( LambdaRuntimeApiSource(..)
  , fromString
  ) where

import RIO hiding (fromString)
import qualified Text.Read
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe

data LambdaRuntimeApiSource = LambdaRuntimeApiSource
  { host :: String
  , port :: Int
  }
  deriving (Eq, Show)

fromString :: String -> Either Parsec.ParseError LambdaRuntimeApiSource
fromString = Parsec.runParser parseFromString () "lambdaRuntimeApiSource"

parseFromString :: Parsec.Parsec String () LambdaRuntimeApiSource
parseFromString = do
  host <- Parsec.manyTill Parsec.anyChar (Parsec.char ':')
  port <- do
    ma <- Text.Read.readMaybe <$> Parsec.many1 Parsec.digit
    Maybe.maybe (fail "Invalid port") pure ma
  pure $ LambdaRuntimeApiSource
    { host = host
    , port = port
    }
