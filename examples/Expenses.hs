module Expenses
  (main) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Prelude
import Data.SpreadSheet -- (SpreadSheet, Pos, toListValues, column, fromList, put)
import Data.SpreadSheet.Cell -- (Cell(..), buildCell, CCell(boxCell), extractDay, extractDouble, toCell)
import Data.SpreadSheet.Date
import Data.Function ((&))
import Data.Maybe (catMaybes)

import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))

data Expense = Expense
               { concepto :: String
               , coste    :: Double
               , fecha    :: Day } deriving (Show, Eq)

instance CompositeCell Expense where
  size _ = 3
  buildCell (Expense con cost fec) = [CString con, CNumber cost, CDay fec]
  buildComposite = buildExpense

instance Arbitrary Expense where
  arbitrary = genExpense

main :: IO ()
main = do
  gen <- QC.generate $ QC.listOf arbitrary :: IO [Expense]
  let ss = toCellAlign Horizontal (1,1) gen
  putStrLn . show $ ss
    & putCell (5,1) (const "Total")
    & putCell (5,2) (const "Octubre")
    & putCell (5,3) (const "Noviembre")
    & putCell (5,4) (const "Diciembre")
    & putCell (6,2) (gastoMes October  . expenses)
    & putCell (6,3) (gastoMes November . expenses)
    & putCell (6,4) (gastoMes December . expenses)
    & putCell (6,1) (gastoTotal . column 6)

expenses :: SpreadSheet Cell -> [Expense]
expenses s = listaDeGastos $ (column 1 s <> column 2 s <> column 3 s)

listaDeGastos :: SpreadSheet Cell -> [Expense]
listaDeGastos s = catMaybes $ fromCellAlign Horizontal s

gastoMes :: Month -> [Expense] -> Double
gastoMes month = sum . map costs . expensesInMonth
  where
    costs = (\(Expense _ cost _) -> cost)
    expensesInMonth = filter (inMonth month)

inMonth :: Month -> Expense -> Bool
inMonth m (Expense _ _ d) = getMonth d == m

gastoTotal :: SpreadSheet Cell -> Double
gastoTotal = sum . extractDouble

--

buildExpense :: [Cell] -> Maybe Expense
buildExpense ((CString con):(CNumber cost):(CDay date):[]) = Just $ Expense con cost date
buildExpense _ = Nothing

genExpense :: QC.Gen Expense
genExpense = do
  n <- QC.choose (0,10)
  concept <- printable <$> QC.resize n arbitrary
  cost <- absdecs <$> arbitrary
  day  <- arbitrary
  return $ Expense concept cost day
    where absdecs = abs . (/ 100) . fromIntegral . round . (* 100) :: Double -> Double
          printable = getPrintableString . PrintableString
