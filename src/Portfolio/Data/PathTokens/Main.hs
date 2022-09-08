module Portfolio.Data.PathTokens.Main
  ( PathTokens
  , fromPath
  , toPath
  , toPieces
  , match
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as List.Split
import qualified Data.Traversable as Traversable
import qualified Portfolio.Data.PathToken.Main as PathToken
import qualified Portfolio.Data.PathConfig.Main as PathConfig

type PathTokens = [PathToken.PathToken]

fromPath :: (PathConfig.HasPathConfig a, Monad m) => String -> Reader.ReaderT a m PathTokens
fromPath xs = do
  config <- PathConfig.get <$> Reader.ask
  pure $ do
    piece <- List.Split.splitOn [config.delimiter] xs
    pure $ (if [config.parameter] `List.isPrefixOf` piece then PathToken.Dynamic else PathToken.Literal) piece

toPath :: (PathConfig.HasPathConfig a, Monad m) => Map.Map String String -> PathTokens -> Reader.ReaderT a m String
toPath params px = do
  config <- PathConfig.get <$> Reader.ask
  List.intercalate [config.delimiter] <$> toPieces params px

toPieces :: (PathConfig.HasPathConfig a, Monad m) => Map.Map String String -> PathTokens -> Reader.ReaderT a m [String]
toPieces params px = Traversable.for px $ PathToken.toPiece params

match :: (PathConfig.HasPathConfig a) => String -> PathTokens -> Reader.ReaderT a Maybe.Maybe (Map.Map String String)
match xs px = do
  config <- PathConfig.get <$> Reader.ask
  let pieces = List.Split.splitOn [config.delimiter] xs
  params <- Traversable.sequence $ List.zipWith PathToken.match pieces px
  pure $ Map.fromList $ Maybe.catMaybes params

