module Lib
  ( SpreadSheet
  , Cell
  , Range
  , Pos
  , range
  , row
  , column
  , extractDouble
  , extractString
  , extractDay
  ) where

import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map.Strict as Map

data Cell = CNumber Double | CString String | CDay Day | Blank deriving (Eq)
data HCell a = HCell a

instance Show Cell where
  show (CNumber x) = show x
  show (CString x) = show x
  show (CDay x)    = show x
  show Blank       = "■"

-- data Cell a = CNumber a | CString a | CDay a | Blank deriving (Eq)

-- data Cell a = Cell a

type Pos = (Int, Int)

data Range = Range Pos Pos deriving Show

type SpreadSheet = Map.Map Pos Cell
type HSpreadSheet a = Map.Map Pos a

{- Seleccionar un rango de una SpreadSheet

Permite definir rangos fijos, como por ejemplo:

a3a4 = range Range (1,3) (1,4) :: Spreadsheet -> Spreadsheet

-}

range :: Range -> SpreadSheet -> SpreadSheet
range r s = Map.intersection s $ genRange r

-- Auxiliary

genRange :: Range -> SpreadSheet
genRange (Range (a1,a2) (b1,b2)) = Map.fromList $ cell <$>
                                   [a1..b1] <*> [a2..b2]
  where cell = \a b -> ((a,b), Blank)

-- Selección de filas/columnas

limit = 100

column :: Int -> SpreadSheet -> SpreadSheet
column n = range $ Range (n,1) (limit, n)

row :: Int -> SpreadSheet -> SpreadSheet
row n = range $ Range (1,n) (n, limit)

{- Mísero intento de abstraer -}

-- extract :: (Cell -> Maybe a) -> SpreadSheet -> HSpreadSheet a
-- extract = Map.mapMaybe

extractDouble :: SpreadSheet -> HSpreadSheet Double
extractDouble = Map.mapMaybe eDouble

eDouble :: Cell -> Maybe Double
eDouble (CNumber x) = Just x
eDouble _ = Nothing

extractString :: SpreadSheet -> HSpreadSheet String
extractString = Map.mapMaybe eString

eString :: Cell -> Maybe String
eString (CString x) = Just x
eString _ = Nothing

extractDay :: SpreadSheet -> HSpreadSheet Day
extractDay = Map.mapMaybe eDay

eDay :: Cell -> Maybe Day
eDay (CDay x) = Just x
eDay _ = Nothing

{- Entradas de ejemplo -}

input :: [(Pos, Cell)]
input = [ ((1,1), CString "Agua")
        , ((1,2), CNumber 1)
        , ((1,3), CDay (fromGregorian 2018 10 10))
        , ((2,1), CString "Zumo")
        , ((2,2), CNumber 2)
        , ((2,3), CDay (fromGregorian 2018 10 10))
        , ((1,5), CNumber 80)
        , ((2,5), CNumber 90)
        ]
input2 :: [(Pos, Cell)]
input2 = [ ((1,1), Blank)
         , ((1,2), Blank)
         , ((1,3), Blank)
         , ((2,1), Blank)
         , ((2,2), Blank)
         , ((2,3), Blank)
         ]
