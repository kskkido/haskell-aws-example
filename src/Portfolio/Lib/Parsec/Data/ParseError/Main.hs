module Portfolio.Lib.Parsec.Data.ParseError.Main
  ( toString
  ) where

import RIO
import qualified RIO.List as List
import qualified Text.Parsec.Error as Parsec.Error

toString :: Parsec.Error.ParseError -> String
toString e = List.intercalate "\n" $ Parsec.Error.messageString <$> Parsec.Error.errorMessages e
