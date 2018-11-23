module Lib ()
  -- ( SpreadSheet
  -- , Cell
  -- , Range
  -- , Pos
  -- , range
  -- , row
  -- , column
  -- , extractDouble
  -- , extractString
  -- , extractDay
  -- ) where
  where

import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map.Strict as Map
import qualified Data.Matrix as Mat
import Data.Bifunctor (bimap)

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

type SpreadSheet a = Map.Map Pos a

-- type HSpreadSheet a = Map.Map Pos a

{- Seleccionar un rango de una SpreadSheet

Permite definir rangos fijos, como por ejemplo:

a3a4 = range Range (1,3) (1,4) :: Spreadsheet -> Spreadsheet

-}

-- range :: Range -> SpreadSheet a -> SpreadSheet a
-- range r s = Map.intersection s $ genRange r

-- -- Auxiliary

-- genRange :: Range -> SpreadSheet Cell
-- genRange (Range (a1,a2) (b1,b2)) = Map.fromList $ cell <$>
--                                    [a1..b1] <*> [a2..b2]
--   where cell = \a b -> ((a,b), Blank)

-- -- Selección de filas/columnas

-- limit = 100

-- column :: Int -> SpreadSheet a -> SpreadSheet a
-- column n = range $ Range (n,1) (limit, n)

-- row :: Int -> SpreadSheet a -> SpreadSheet a
-- row n = range $ Range (1,n) (n, limit)

get :: (Pos -> Bool) -> SpreadSheet a -> SpreadSheet a
get f s = fst $ (Map.partitionWithKey (\a _ -> f a)) s

range :: Range -> SpreadSheet a -> SpreadSheet a
range (Range (ic, ir) (fc, fr)) s
  | ic >= 0 && ir >= 0 && ic <= fc && ir <= fr = go
  | otherwise = Map.empty
  where go = get (\(c,r) -> ic <= c && ir <= r && fc >= c && fr >= r) s

column :: Int -> SpreadSheet a -> SpreadSheet a
column n = get (\(c,r) -> c == n)

row :: Int -> SpreadSheet a -> SpreadSheet a
row n = get (\(c,r) -> r == n)

{- Mísero intento de abstraer -}

extract :: (Cell -> Maybe a) -> SpreadSheet Cell -> SpreadSheet a
extract = Map.mapMaybe

extractDouble :: SpreadSheet Cell -> SpreadSheet Double
extractDouble = extract eDouble

eDouble :: Cell -> Maybe Double
eDouble (CNumber x) = Just x
eDouble _ = Nothing

extractString :: SpreadSheet Cell -> SpreadSheet String
extractString = extract eString

eString :: Cell -> Maybe String
eString (CString x) = Just x
eString _ = Nothing

extractDay :: SpreadSheet Cell -> SpreadSheet Day
extractDay = extract eDay

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

-- pretty :: SpreadSheet Cell -> String
-- pretty s = Mat.prettyMatrix mat
--   where mat  = Mat.fromList (div n cols) cols ls
--         ls   = map snd $ Map.toList (Map.union s r)
--         n    = Map.size r
--         cols = snd end
--         init = (fst bm)
--         end  = (snd bm)
--         r    = genRange $ Range init end
--         bm   = bimap (fh . Map.toAscList) (fh . Map.toDescList) $ (s, s)
--         fh   = (fst . head)
