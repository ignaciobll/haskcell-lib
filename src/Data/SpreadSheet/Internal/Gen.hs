module Data.SpreadSheet.Internal.Gen where


import Data.SpreadSheet (SpreadSheet, Range(..), fromList, toListValues, toListPosValues)
import Data.SpreadSheet.Cell (Cell(..))
import Data.Time.Calendar (Day(..))

import Numeric.Natural
import qualified Data.Map.Strict as Map

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function

import Control.Applicative (liftA2)

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Integer)
  -- shrink nat = fromIntegral . abs <$> shrink (toInteger nat) :: [Natural]

instance Arbitrary Range where
  arbitrary = liftA2 Range arbitrary arbitrary

instance Arbitrary a => Arbitrary (SpreadSheet a) where
  arbitrary = fromList <$> arbitrary

instance CoArbitrary a => CoArbitrary (SpreadSheet a) where
  coarbitrary = coarbitrary . toListValues

instance CoArbitrary Natural where
  coarbitrary = coarbitrary . toInteger

instance Function Natural where
  function = functionMap toInteger fromInteger

-- instance Function Cell where
--   function = functionShow

-- instance CoArbitrary Cell where
--   coarbitrary = coarbitraryShow

instance Function a => Function (SpreadSheet a) where
  function = functionMap (Map.fromList . toListPosValues) (fromList . Map.toList)

instance Arbitrary Day where
  arbitrary = genDay

instance Arbitrary Cell where
  -- shrink = const []
  arbitrary = frequency $
    [ (1, CNumber <$> arbitrary)
    , (1, CString <$> arbitrary)
    , (1, CBool <$> arbitrary)
    , (1, CDay <$> arbitrary)
    ]


genDay :: Gen Day
genDay = do
  n <- (58470 +) <$> arbitrary
  return $ ModifiedJulianDay n
