-- | Conjunto de generadore e instancias de
-- 'Test.QuickCheck.Arbitrary' para
-- 'Test.QuickCheck.QuickCheck'. Útiles para el ejemplo y para los
-- módulos de test.
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

-- | Instancia para los naturales, igual que para 'Integer' pero solo
-- de sus valores absolutos.
instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Integer)
  -- shrink nat = fromIntegral . abs <$> shrink (toInteger nat) :: [Natural]

-- | Se deriva la instancia de 'Natural'
instance Arbitrary Range where
  arbitrary = liftA2 Range arbitrary arbitrary

-- | Se deriva la instancia de @[(Pos, a)]@
instance Arbitrary a => Arbitrary (SpreadSheet a) where
  arbitrary = fromList <$> arbitrary

-- | Tomada a partir de la lista de valores. @[a]@
instance CoArbitrary a => CoArbitrary (SpreadSheet a) where
  coarbitrary = coarbitrary . toListValues

-- | Tomada a partir de los 'Integer'
instance CoArbitrary Natural where
  coarbitrary = coarbitrary . toInteger

-- | Se obtiene a través de los 'Integer'
instance Function Natural where
  function = functionMap toInteger fromInteger

-- instance Function Cell where
--   function = functionShow

-- instance CoArbitrary Cell where
--   coarbitrary = coarbitraryShow

-- | Tomada a partir de las listas de valores con posiciones.
instance Function a => Function (SpreadSheet a) where
  function = functionMap (Map.fromList . toListPosValues) (fromList . Map.toList)

-- | Usando 'genDay'.
instance Arbitrary Day where
  arbitrary = genDay

-- | Distribuye de forma equitativa la generación de valores:
-- - 'Data.SpreadSheet.Cell.CNumber'
-- - 'Data.SpreadSheet.Cell.CString'
-- - 'Data.SpreadSheet.Cell.CBool'
-- - 'Data.SpreadSheet.Cell.CDay'
instance Arbitrary Cell where
  -- shrink = const []
  arbitrary = frequency $
    [ (1, CNumber <$> arbitrary)
    , (1, CString <$> arbitrary)
    , (1, CBool <$> arbitrary)
    , (1, CDay <$> arbitrary)
    ]

-- | Generador de fechas próximas al final de 2018.
genDay :: Gen Day
genDay = do
  n <- (58470 +) <$> arbitrary
  return $ ModifiedJulianDay n
