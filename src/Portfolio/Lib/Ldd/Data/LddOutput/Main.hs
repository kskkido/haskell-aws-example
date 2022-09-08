module Portfolio.Lib.Ldd.Data.LddOutput.Main
  ( LddOutput(..)
  , toSharedLibraries
  , fromOutput
  , parseFromOutput
  ) where

import RIO
import qualified Data.Maybe as Maybe
import qualified Text.Parsec as Parsec
import qualified Portfolio.Lib.Ldd.Data.SharedLibrary.Main as SharedLibrary

newtype LddOutput = LddOutput [SharedLibrary.SharedLibrary]
  deriving (Eq, Show, Generic)

toSharedLibraries :: LddOutput -> [SharedLibrary.SharedLibrary]
toSharedLibraries (LddOutput xs) = xs

fromOutput :: (Parsec.Stream s m Char) => s -> m (Either Parsec.ParseError LddOutput)
fromOutput = Parsec.runParserT parseFromOutput () "ldd-output"

parseFromOutput :: (Parsec.Stream s m Char) => Parsec.ParsecT s () m LddOutput
parseFromOutput = do
  libraries <- Maybe.catMaybes <$>
    ( ( Parsec.choice
        [ Parsec.try do
            Parsec.spaces
            filename <- Parsec.many1 (Parsec.noneOf " ")
            Parsec.spaces
            void $ Parsec.string "=>"
            Parsec.spaces
            filepath <- Parsec.many1 (Parsec.noneOf " (")
            Parsec.spaces
            void $ Parsec.manyTill Parsec.anyChar (Parsec.char '(' )
            address <- Parsec.manyTill (Parsec.noneOf " ") (Parsec.char ')' )
            Parsec.spaces
            pure $ Just $ SharedLibrary.SharedLibrary
              { SharedLibrary.filename = filename
              , SharedLibrary.filepath = filepath
              , SharedLibrary.address = address
              }
        , Parsec.try do
            void $ Parsec.manyTill Parsec.anyChar Parsec.endOfLine
            pure Nothing
        ]
      ) &
      ( `Parsec.sepBy` (Parsec.spaces <|> (Parsec.endOfLine $> ())) )
    )
  void $ Parsec.manyTill Parsec.anyChar Parsec.eof
  pure $ LddOutput libraries
