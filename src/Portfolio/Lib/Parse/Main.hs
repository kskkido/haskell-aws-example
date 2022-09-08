module Portfolio.Lib.Parse.Main
  ( parseInt
  , parseWord16
  ) where

import RIO

parseInt :: String -> Maybe Int
parseInt = readMaybe

parseWord16 :: String -> Maybe Word16
parseWord16 = fmap (fromInteger . toInteger) . readMaybe

