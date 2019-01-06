{-# LANGUAGE ViewPatterns #-}

module Data.SpreadSheet.CellSpec where

import Data.SpreadSheet
import Data.SpreadSheet.Cell
import Data.SpreadSheet.Cell (Align(..))
import Data.SpreadSheet.Internal.Gen

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function hiding (apply)
import qualified Test.QuickCheck.Function as TQF (apply)


import Test.Hspec.Checkers
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA3)
import Data.Maybe (catMaybes)

import Data.Time.Calendar (Day(..))
import Numeric.Natural
import Data.List (elem, sort)
import qualified Data.Map.Strict as Map

data Expense = Expense
               { concepto :: String
               , coste    :: Double
               , fecha    :: Day } deriving (Show, Eq)

instance CompositeCell Expense where
  size _ = 3
  buildCell (Expense con cost fec) = [CString con, CNumber cost, CDay fec]
  buildComposite  ((CString con):(CNumber cost):(CDay date):[]) = Just $ Expense con cost date
  buildComposite  _ = Nothing

instance Arbitrary Expense where
  arbitrary = liftA3 Expense arbitrary arbitrary arbitrary

instance EqProp Expense where
  (=-=) = eq

spec :: Spec
spec = do
  describe "fromCellAlign and toCellAlign" $ do
    context "vertical align" $ do
      prop "are left inverse" $ do
        (inverseL
         (fromCellAlignVerticalAtOneOne :: SpreadSheet Cell -> [Expense])
         (toCellAlignVerticalAtOneOne :: [Expense] -> SpreadSheet Cell))

    context "horizontal align" $ do
      prop "are left inverse" $ do
        (inverseL
         (fromCellAlignHorizontalAtOneOne :: SpreadSheet Cell -> [Expense])
         (toCellAlignHorizontalAtOneOne :: [Expense] -> SpreadSheet Cell))

  describe "extractDouble" $ do
    prop "gen all double values" $ do
      (prop_extract
        (\cell -> case cell of CNumber x -> True; _ -> False)
        (\cell -> case cell of CNumber x -> x)
        extractDouble) :: SpreadSheet Cell -> Bool

  describe "extractString" $ do
    prop "gen all string values" $ do
      (prop_extract
        (\cell -> case cell of CString x -> True; _ -> False)
        (\cell -> case cell of CString x -> x)
        extractString) :: SpreadSheet Cell -> Bool

  describe "extractDay" $ do
    prop "gen all Day values" $ do
      (prop_extract
        (\cell -> case cell of CDay x -> True; _ -> False)
        (\cell -> case cell of CDay x -> x)
        extractDay) :: SpreadSheet Cell -> Bool

  describe "extractBool" $ do
    prop "gen all Bool values" $ do
      (prop_extract
        (\cell -> case cell of CBool x -> True; _ -> False)
        (\cell -> case cell of CBool x -> x)
        extractBool) :: SpreadSheet Cell -> Bool

  describe "putCell" $ do
    prop "should be equivalent to put" $ do
      prop_putCellDouble
        .&&. prop_putCellBool
        .&&. prop_putCellString
        .&&. prop_putCellDay

  describe "show (overlap)" $ do
    prop "should show all elements" $ do
      forAll (arbitrary :: Gen (SpreadSheet Cell)) show_rel


toCellAlignVerticalAtOneOne :: CompositeCell a => [a] -> SpreadSheet Cell
toCellAlignVerticalAtOneOne = toCellAlign Vertical (1,1)

fromCellAlignVerticalAtOneOne :: CompositeCell a => SpreadSheet Cell -> [a]
fromCellAlignVerticalAtOneOne = catMaybes . (fromCellAlign Vertical)

toCellAlignHorizontalAtOneOne :: CompositeCell a => [a] -> SpreadSheet Cell
toCellAlignHorizontalAtOneOne = toCellAlign Horizontal (1,1)

fromCellAlignHorizontalAtOneOne :: CompositeCell a => SpreadSheet Cell -> [a]
fromCellAlignHorizontalAtOneOne = catMaybes . (fromCellAlign Horizontal)

show_rel :: SpreadSheet Cell -> Property
show_rel s
  | s == empty = property $ (show s) == "Empty"
  | otherwise = property $ all (\value -> value == "unprintabblestring" || value `elem` visual) values
  where values = map showIfNotString $ toListValues s
        visual = words $ show s
        showIfNotString = \cell -> case cell of
                                     CString x -> "unprintabblestring"
                                     x -> show x

prop_putCellDouble :: Pos -> Double -> SpreadSheet Cell -> Bool
prop_putCellDouble pos val s = (put pos (\x -> CNumber $ f s) s) == (putCell pos f s)
  where f = const val

prop_putCellBool :: Pos -> Bool -> SpreadSheet Cell -> Bool
prop_putCellBool pos val s = (put pos (\x -> CBool $ f s) s) == (putCell pos f s)
  where f = const val

prop_putCellDay :: Pos -> Day -> SpreadSheet Cell -> Bool
prop_putCellDay pos val s = (put pos (\x -> CDay $ f s) s) == (putCell pos f s)
  where f = const val

prop_putCellString :: Pos -> String -> SpreadSheet Cell -> Bool
prop_putCellString pos val s = (put pos (\x -> CString $ f s) s) == (putCell pos f s)
  where f = const val

prop_putCell :: Pos -> Fun (SpreadSheet Cell) Double -> SpreadSheet Cell -> Bool
prop_putCell pos (TQF.apply -> f) s = (put pos (\x -> CNumber $ f s) s) == (putCell pos f s)

prop_extract :: Ord a => (Cell -> Bool) -> (Cell -> a) -> (SpreadSheet Cell -> SpreadSheet a) -> SpreadSheet Cell -> Bool
prop_extract f extractV extractF s = sort (map extractV $ filter f $ toListValues s) == (sort . toListValues $ extractF s)
