module Portfolio.Data.PathToken.Main
  ( PathToken(..)
  , toPiece
  , match
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Portfolio.Data.PathConfig.Main as PathConfig

data PathToken =
    Literal String
  | Dynamic String

toPiece :: (PathConfig.HasPathConfig a, Monad m) => Map.Map String String -> PathToken -> Reader.ReaderT a m String
toPiece params px = do
  config <- PathConfig.get <$> Reader.ask
  pure $ case px of
    Literal xs -> xs
    Dynamic xs -> Maybe.fromMaybe "" $ do
      let key = List.dropWhile (== config.parameter) xs
      Map.lookup key params

match :: (PathConfig.HasPathConfig a) => String -> PathToken -> Reader.ReaderT a Maybe.Maybe (Maybe.Maybe (String, String))
match piece (Literal xs) = lift $ do
  Control.Monad.guard (piece == xs)
  pure (Maybe.Nothing)
match piece (Dynamic xs) = do
  config <- PathConfig.get <$> Reader.ask
  lift $ do
    let key = List.dropWhile (== config.parameter) xs
    pure (pure (key, piece))

