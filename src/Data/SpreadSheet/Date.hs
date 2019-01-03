module Data.SpreadSheet.Date
  ( Month(..)
  , WeekDay(..)
  , between
  , next
  , getMonth
  ) where

import Test.QuickCheck (Arbitrary(..), Gen(..))
import Data.Time.Calendar (Day(..), fromGregorian, toGregorian, gregorianMonthLength)
import Data.Ix (Ix(..))

between :: Day -> Day -> Day -> Bool
between d d1 d2 = d1 <= d && d <= d2

instance Arbitrary Day where
  arbitrary = genDay

genDay :: Gen Day
genDay = do
  n <- (58450 +) <$> arbitrary
  return $ ModifiedJulianDay n

next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a

getMonth :: Day -> Month
getMonth date = toEnum month
  where (_year, month, _day) = toGregorian date

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Ord, Bounded, Ix, Read)

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Ord, Bounded, Ix, Read)

instance Enum Month where
  fromEnum January = 1
  fromEnum February = 2
  fromEnum March = 3
  fromEnum April = 4
  fromEnum May = 5
  fromEnum June = 6
  fromEnum July = 7
  fromEnum August = 8
  fromEnum September = 9
  fromEnum October = 10
  fromEnum November = 11
  fromEnum December = 12

  toEnum 1 = January
  toEnum 2 = February
  toEnum 3 = March
  toEnum 4 = April
  toEnum 5 = May
  toEnum 6 = June
  toEnum 7 = July
  toEnum 8 = August
  toEnum 9 = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December

instance Enum WeekDay where
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
  fromEnum Sunday = 7

  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday
