module Data.SpreadSheet.Date (
  between
  ) where

import Test.QuickCheck (Arbitrary(..), Gen(..))
import Data.Time.Calendar (Day(..), fromGregorian, toGregorian, gregorianMonthLength)

between :: Day -> Day -> Day -> Bool
between d d1 d2 = d1 <= d && d <= d2

instance Arbitrary Day where
  arbitrary = genDay

genDay :: Gen Day
genDay = do
  n <- (58450 +) <$> arbitrary
  return $ ModifiedJulianDay n
