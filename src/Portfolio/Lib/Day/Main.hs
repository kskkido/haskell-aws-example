module Portfolio.Lib.Day.Main
  ( Time.Calendar.Day(..)
  , toFirstDayOfMonth
  , toMonthString
  , toString
  ) where

import RIO
import qualified RIO.List as List
import qualified Data.Time.Calendar as Time.Calendar

toFirstDayOfMonth :: Time.Calendar.Day -> Time.Calendar.Day
toFirstDayOfMonth day =
  let (year, month, _) = Time.Calendar.toGregorian day
   in Time.Calendar.fromGregorian year month 1

toMonthString :: Time.Calendar.Day -> String
toMonthString day =
  let (year, month, _) = Time.Calendar.toGregorian day
   in show year <> "-" <> show month

toString :: Time.Calendar.Day -> String
toString day =
  let (year, month, date) = Time.Calendar.toGregorian day
   in
     ( ( [show year, show month, show date] ) &
       ( List.intercalate "-" )
     )
