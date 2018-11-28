module Cell where

import Lib

import Data.Time.Calendar (Day, fromGregorian)

data Cell = CNumber Double | CString String | CDay Day | Blank deriving (Eq)

instance Show Cell where
  show (CNumber x) = show x
  show (CString x) = show x
  show (CDay x)    = show x
  show Blank       = "■"

instance Celldable Cell where
  blank = Blank

{- Entradas de ejemplo -}

input :: [(Pos, Cell)]
input = [ ((1, 1), CString "Agua")
        , ((2, 1), CNumber 1)
        , ((3, 1), CDay (fromGregorian 2018 10 10))
        , ((1, 2), CString "Zumo")
        , ((2, 2), CNumber 2)
        , ((3, 2), CDay (fromGregorian 2018 10 10))
        , ((5, 1), CNumber 80)
        , ((5, 2), CNumber 90)
        ]
input2 :: [(Pos, Cell)]
input2 = [ ((1,1), Blank)
         , ((1,2), Blank)
         , ((1,3), Blank)
         , ((2,1), Blank)
         , ((2,2), Blank)
         , ((2,3), Blank)
         ]

input3 :: [(Pos, Cell)]
input3 = [ ((6,6), CNumber 3)
         , ((3,5), CString "Luishnardo")
         ]

{- Mísero intento de abstraer -}

extractDouble :: SpreadSheet Cell -> SpreadSheet Double
extractDouble = extract byDouble
  where byDouble (CNumber x) = Just x
        byDouble _ = Nothing

extractString :: SpreadSheet Cell -> SpreadSheet String
extractString = extract byString
  where byString (CString x) = Just x
        byString _ = Nothing

extractDay :: SpreadSheet Cell -> SpreadSheet Day
extractDay = extract byDay
  where byDay (CDay x) = Just x
        byDay _ = Nothing

-- extractG :: (a -> Cell) -> SpreadSheet Cell -> SpreadSheet a
-- extractG f s = extract byBuilder
--   where byBuilder (f x) = Just x
--         byBuilder _ = Nothing
