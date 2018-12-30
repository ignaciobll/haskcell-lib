{-# LANGUAGE FlexibleInstances #-}

module Data.SpreadSheet.Cell where

import Data.SpreadSheet
import Data.SpreadSheet (Range(..), SpreadSheet(..))
import Data.SpreadSheet.Internal.Pretty

import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map.Strict as Map

data Cell = CNumber Double | CString String | CDay Day | Blank deriving (Eq)

instance Show Cell where
  show (CNumber x) = show x
  show (CString x) = show x
  show (CDay x)    = show x
  show Blank       = "■"

instance {-# OVERLAPS #-} Show (SpreadSheet Cell) where
  show = prettyShowSpreadSheet

class CCell a  where
  boxCell :: a -> Cell

instance CCell Day where
  boxCell a = CDay a

instance CCell Double where
  boxCell a = CNumber a

instance CCell String where
  boxCell a = CString a

instance Celldable Cell where
  blank = Blank

class ComplexCell a where
  size :: a -> Int
  buildCell :: a -> [Cell]

{- Mísero intento de abstraer -}

extractDouble :: SpreadSheet Cell -> SpreadSheet Double
extractDouble = mapMaybe byDouble
  where byDouble (CNumber x) = Just x
        byDouble _ = Nothing

extractString :: SpreadSheet Cell -> SpreadSheet String
extractString = mapMaybe byString
  where byString (CString x) = Just x
        byString _ = Nothing

extractDay :: SpreadSheet Cell -> SpreadSheet Day
extractDay = mapMaybe byDay
  where byDay (CDay x) = Just x
        byDay _ = Nothing

-- extractG :: (a -> Cell) -> SpreadSheet Cell -> SpreadSheet a
-- extractG f s = extract byBuilder
--   where byBuilder (f x) = Just x
--         byBuilder _ = Nothing

-- Put

number :: Double -> SpreadSheet Cell -> Cell
number d = (\_ -> CNumber d)

string :: String -> SpreadSheet Cell -> Cell
string s = (\_ -> CString s)

--

data Align = Horizontal | Vertical

genPos :: ComplexCell a => a -> Pos -> [Pos]
genPos = genPosAlign Horizontal

genPosAlign :: ComplexCell a => Align -> a -> Pos -> [Pos]
genPosAlign Horizontal e (x, y) = [(x+i, y) | i <- [0..(size e - 1)]]
genPosAlign Vertical   e (x, y) = [(x, y+i) | i <- [0..(size e - 1)]]

toCell :: ComplexCell a => Pos -> [a] -> [(Pos, Cell)]
toCell (col, row) xs = mconcat $ zipWith toPosCell range xs
  where range = [(col, row + i) | i <- [0..(length xs - 1)]]

toPosCell :: ComplexCell a => Pos -> a -> [(Pos, Cell)]
toPosCell pos x = zip (genPos x pos) (buildCell x)

-- toCellAlign :: Align -> Pos -> [Expense] -> [(Pos, Cell)]
-- toCellAlign Horizontal p ls = undefined
