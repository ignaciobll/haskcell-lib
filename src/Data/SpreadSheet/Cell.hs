{-# LANGUAGE FlexibleInstances #-}

-- | Este módulo permita una utilización más convencional de las hojas
-- de cálculo. Ofrece el tipo de datos necesario para poder tener
-- valores heterogéneos (de distinto tipo) sobre cada celda.
module Data.SpreadSheet.Cell
  ( Cell(..)
  , putCell
  -- * Extracción
  -- $extract
  , extractDouble
  , extractString
  , extractDay
  , extractBool
  -- * Tipos complejos
  , CompositeCell(..)
  , Align(..)
  , toCellAlign
  , fromCellAlign
  ) where

import Data.SpreadSheet
import Data.SpreadSheet (Range(..), SpreadSheet(..))
import Data.SpreadSheet.Internal.Pretty
import Numeric.Natural (Natural(..))

import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map.Strict as Map

-- | El tipo de dato 'Cell' es la suma de los tipos 'Double',
-- 'String', 'Day' y 'Bool'. Además, se añade el valor 'Blank' para
-- poder explicitar celdas vacías. Las hojas de cálculo que se
-- crearían serían del tipo 'SpreadSheet' 'Cell'.
--
-- Si queremos trabajar con los valores básicos de cada celda se puede
-- definir una función sobre el dominio de 'Cell' o bien extraer el
-- propio valor. Para hacer esto último se ofrece la familia de
-- funciones @extract@, como por ejemplo 'extractDouble'. Estas
-- funciones generarían una hoja de cálculo homogenea con solo las
-- celdas que sean del tipo correspondiente.
-- /(Ejemplos en la definición de estas funciones)/.
--
-- Se ofrecen también las funciones para generar tipos complejos de
-- datos a partir de conjuntos de celdas con la clase 'ComplexCell' y
-- las funciones 'toCell' y 'fromCells'.
data Cell = CNumber Double
          | CString String
          | CDay Day -- ^ Se ofrece el módulo
                     -- 'Data.SpreadSheet.Date.Date' con ciertas
                     -- funciones para facilitar operaciones con
                     -- fechas.
          | CBool Bool
          | Blank deriving (Eq)

instance Show Cell where
  show (CNumber x) = show x
  show (CString x) = show x
  show (CDay x)    = show x
  show (CBool x)   = show x
  show Blank       = "■"

instance Celldable Cell where
  blank = Blank

-- | 'Cell' instancia la clase 'Celldable' y utiliza la función
-- 'Data.SpreadSheet.Internal.Pretty.prettyShowSpreadSheet para hacer
-- su representación.
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

instance CCell Bool where
  boxCell a = CBool a

-- | Conjuntos de celdas se pueden representar atributos de otros
-- datos compuestos. Mediante la implementación de esta clase se
-- ofrecen herramientas para componer y descomponer conjuntos de
-- datos.
class CompositeCell a where
  -- | El númmero de celdas necesarias para representar un dato @a@
  -- puede variar, por eso siempre se toma como parámetro para indicar
  -- el tamaño.
  --
  -- > data Localización = Coordenada { lat :: Double, lon :: Double }
  -- >                   | Dirección { calle :: String, número :: Int,
  -- >                                 ciudad :: String
  -- >                               }
  --
  -- Para el caso de una @Localización@ podríamos tener distintas
  -- formas de representarla, y por tanto, varios tamaños.
  --
  -- >>> size $ Coordenada 40.404823 -3.839532
  -- 2
  -- >>> size $ Dirección "Avd. de los Ciruelos" 0 "Boadilla del Monte"
  -- 3
  size :: a -> Natural
  buildCell :: a -> [Cell]
  buildComposite :: [Cell] -> Maybe a

{- Mísero intento de abstraer -}

-- $extract
-- La funciones 'extractDouble', 'extractString', 'extractDay' y
-- 'extractBool' pertenecen a la misma familia de funciones que toman
-- una hoja de cálculo de 'Cell' y devuelven una nueva hoja con solo
-- los valores deseados.

-- |
extractDouble :: SpreadSheet Cell -> SpreadSheet Double
extractDouble = mapMaybe byDouble
  where byDouble (CNumber x) = Just x
        byDouble _ = Nothing

-- |
extractString :: SpreadSheet Cell -> SpreadSheet String
extractString = mapMaybe byString
  where byString (CString x) = Just x
        byString _ = Nothing

-- |
extractDay :: SpreadSheet Cell -> SpreadSheet Day
extractDay = mapMaybe byDay
  where byDay (CDay x) = Just x
        byDay _ = Nothing

-- |
extractBool :: SpreadSheet Cell -> SpreadSheet Bool
extractBool = mapMaybe byBool
  where byBool (CBool x) = Just x
        byBool _ = Nothing

-- GADTS will fix this
--        !
--        v
-- extractG :: (a -> Cell) -> SpreadSheet Cell -> SpreadSheet a
-- extractG f s = extract byBuilder
--   where byBuilder (f x) = Just x
--         byBuilder _ = Nothing

-- | Se restringe la función 'Data.SpreadSheet.put' para solo valores
-- de 'Cell'. Además, permite incluir directamente funciones de
-- @SpreadSheet Cell -> a@ siempre que @a@ pueda ser un valor en 'Cell'.
--
-- >>> put (1,1) (const $ CString "valor") empty
-- Range (1,1) (1,1)
-- ┌         ┐
-- │ "valor" │
-- └         ┘
-- >>> putCell (1,1) (const "valor") empty
-- Range (1,1) (1,1)
-- ┌         ┐
-- │ "valor" │
-- └         ┘
--
-- >>> let cells = fromList [((1,1), CBool True), ((1,2), CBool False)]
-- >>> put (2,1) (CNumber . fromIntegral . length) (column 1 cells)
-- ┌             ┐
-- │  True   2.0 │
-- │ False     ■ │
-- └             ┘
-- >>> putCell (2,1) (fromIntegral . length) (column 1 cells)
-- Range (1,1) (2,2)
-- ┌             ┐
-- │  True   2.0 │
-- │ False     ■ │
-- └             ┘
putCell :: CCell a => Pos -> (SpreadSheet Cell -> a) -> SpreadSheet Cell -> SpreadSheet Cell
putCell p f s = put p (boxCell . f) s


-- | Para la construcción de tipos complejos es necesario indicar la
-- orientación de datos.
--
-- Si por ejemplo se desea constuir un tipo complejo como es 'Expense':
--
-- @
-- data Expense = { concepto :: String, coste :: Double, fecha :: Day }
-- @
--
-- Los datos pueden estar dispuestos de dos formas diferentes:
--
-- __1__ Horizontal:
--
-- > ┌                                  ┐
-- > │ "Concepto"    "Coste"    "Fecha" │
-- > │     "Agua"        1.0 2018-10-10 │
-- > │     "Zumo"        2.0 2018-10-10 │
-- > └                                  ┘
--
-- __2__ Vertical:
--
-- > ┌                                  ┐
-- > │ "Concepto"     "Agua"     "Zumo" │
-- > │ "Coste"           1.0        2.0 │
-- > │ "Fecha"    2018-10-10 2018-10-10 │
-- > └                                  ┘
data Align = Horizontal | Vertical deriving (Show, Eq)

-- | Transforma un dato complejo a un conjunto de celdas. La
-- orientación se representa como en 'Align'.
--
-- Es necesario que estos datos implementen la clase 'CompositeCell'
toCellAlign :: CompositeCell a => Align -> Pos -> [a] -> SpreadSheet Cell
toCellAlign align (x,y) listCC
  | align == Horizontal = mconcat $ zipWith join rowIndex listCC
  | align == Vertical   = mconcat $ zipWith join colIndex listCC
  where join l c = fromList $ zipWith (,) l (buildCell c)
        colIndex = map (\col -> [(x + col, y + i) | i <- [0..]]) [0..]
        rowIndex = map (\row -> [(x + i, y + row) | i <- [0..]]) [0..]

-- | Obtiene posibles datos complejos de un conjunto de celdas.
fromCellAlign :: CompositeCell a => Align -> SpreadSheet Cell -> [Maybe a]
fromCellAlign _ s | s == empty = []
fromCellAlign Horizontal s = map rowToCompositeCell (rows s)
  where rowToCompositeCell n = buildComposite $ toListValues (row n s)
fromCellAlign Vertical s = map columnToCompositeCell (columns s)
  where columnToCompositeCell n = buildComposite $ toListValues (column n s)
