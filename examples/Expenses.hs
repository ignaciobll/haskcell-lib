module Expenses
  (main) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Prelude
import Data.SpreadSheet -- (SpreadSheet, Pos, toListValues, column, fromList, put)
import Data.SpreadSheet.Cell -- (Cell(..), ComplexCell(..), buildCell, CCell(boxCell), extractDay, extractDouble, toCell)
import Data.SpreadSheet.Date (between)
import Data.Function ((&))

import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))

data Expense = Expense
               { concepto :: String
               , coste    :: Double
               , fecha    :: Day } deriving (Show, Eq)

putCell :: CCell a => Pos -> (SpreadSheet Cell -> a) -> SpreadSheet Cell -> SpreadSheet Cell
putCell p f s = put p (boxCell . f) s

main :: IO ()
main = do
  gen <- QC.generate $ QC.listOf arbitrary :: IO [Expense]
  let ss = fromList $ toCell (1,1) gen
  putStrLn . show $ ss
    & putCell (5,1) (const "Total")
    & putCell (5,2) (const "Octubre")
    & putCell (5,3) (const "Noviembre")
    & putCell (5,4) (const "Diciembre")
    & putCell (6,2) (gastoMes (fromGregorian 2018 10 1))
    & putCell (6,3) (gastoMes (fromGregorian 2018 11 1))
    & putCell (6,4) (gastoMes (fromGregorian 2018 12 1))
    & putCell (6,1) (gastoTotal . column 6)

gastoTotal :: SpreadSheet Cell -> Double
gastoTotal = sum . extractDouble

gastoMes :: Day -> SpreadSheet Cell -> Double
gastoMes d s = sum $ map snd $ filter (cond . fst) pair
  where
    (year, month, _day) = toGregorian d
    beginMonth = fromGregorian year month 1
    endMonth = fromGregorian year month (gregorianMonthLength year month)
    cond = \day -> between day beginMonth endMonth
    pair = zipWith (flip (,)) (toListValues . extractDouble $ column 2 s) (toListValues . extractDay $ column 3 s)

instance ComplexCell Expense where
  size _ = 3
  buildCell (Expense con cost fec) = [CString con, CNumber cost, CDay fec]

instance Arbitrary Expense where
  arbitrary = genExpense

genExpense :: QC.Gen Expense
genExpense = do
  n <- QC.choose (0,10)
  concept <- printable <$> QC.resize n arbitrary
  cost <- absdecs <$> arbitrary
  day  <- arbitrary
  return $ Expense concept cost day
    where absdecs = abs . (/ 100) . fromIntegral . round . (* 100) :: Double -> Double
          printable = getPrintableString . PrintableString
