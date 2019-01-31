--   El uso Unsafe viene de import Data.Matrix. Ese módulo solo se usa
-- para hacer pretty printing.

{-# LANGUAGE Safe #-}

-- | Módulo general para hojas de cálculo de la herramienta @haskcell@.
module Data.SpreadSheet
  ( SpreadSheet(..) -- Para reescribir en Data.SpreadSheet.Internal
  , Pos
  -- , Pos'(..)
  , Range(..)
  -- * Constructores
  , empty
  , singleton
  , singletonPos
  , fromList
  , toListValues
  , toListPosValues
  , fromDict
  -- * Selección y referencias
  , get
  , selectRange
  , row
  , column
  , select
  -- * Modificación
  , put
  , union
  , intersection
  , (</>)
  , mapMaybe
  -- * Propiedades derivadas
  , width
  , height
  , limits
  , columns
  , rows
  ) where

import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.Bifunctor (bimap)
import Data.Tuple (swap)
import Data.Ix (Ix(..))
import Numeric.Natural (Natural(..))

-- | Posición de una celda en la hoja de cálculo. Comienzan en @(1,1)@.
--
-- Punto bidimensional en el que la primera coordenada corresponde a
-- la columna y la segunda a la fila.
--
-- Ejemplo para localizar una celda:
--
-- +------+----------+
-- | @A3@ | @(1,3)@  |
-- +------+----------+
-- | @J9@ | @(10,9)@ |
-- +------+----------+

type Pos = (Natural, Natural)

-- data Pos' = Pos'
--   ( Int -- ^ Columna
--   , Int -- ^ Fila
--   ) deriving (Show, Eq, Ord, Ix)


-- | Un rango está definido por dos posiciones, la superior izquierda
-- y la inferior derecha.
--
-- Es utilizado en la generación de rangos con la función 'range'.
data Range = Range Pos Pos deriving (Show, Eq, Ord, Ix)

-- | 'SpreadSheet'  representa la estructura  de datos de una  hoja de
-- cálculo.
--
-- Las hojas cálculo pueden contener cualquier tipo de dato, pero solo
-- valores de dicho tipo. Es decir, no se podrán tener hojas de
-- cálculo heterogéneas que contengan valores 'Double' o 'Bool' a la
-- vez. Para una representación más cercana al uso ordinario de las
-- hojas de cálculo se recomienda mirar el módulo
-- 'Data.SpreadSheet.Cell.Cell'.
--
-- La implementación interna de esta estructura de datos es mediante
-- un diccionario ('Data.Map.Map' 'Data.Map.Strict') donde las claves son posiciones ('Pos').
--
-- En este módulo se definen un conjunto de operaciones para la
-- creación, consulta y modificación de hojas de cálculo.
data SpreadSheet a = Mk { topl :: Pos  -- Top Left Position
                        , botr :: Pos  -- Bottom Right Position
                        , mp   :: Map.Map Pos a } | Empty

-- | La igualdad entre dos hojas de cálculo se produce cuando el valor
-- de todas sus celdas son iguales y cuando hacen referencia al mismo
-- rango de posiciones.
instance Eq a => Eq (SpreadSheet a) where
  Empty == Empty = True
  (Mk tl1 br1 s1) == (Mk tl2 br2 s2) = (tl1 == tl2) && (br1 == br2) && (s1 == s2)
  _ == _ = False

instance Functor SpreadSheet where
  fmap f Empty = Empty
  fmap f (Mk tl br s) = Mk tl br (Map.map f s)


-- | Operación binaria asociativa implementada con 'union'.
instance Semigroup (SpreadSheet a) where
  (<>) = union

instance Monoid (SpreadSheet a) where
  mempty = Empty

instance Foldable SpreadSheet where
  foldr _ z Empty  = z
  foldr f z (Mk _ _ s) = Map.foldr f z s
  toList = toListValues

-- | Consultar
-- 'Data.SpreadSheet.Internal.Pretty.prettyShowSpreadSheet' para una
-- visualización más avanzada.
instance {-# OVERLAPPABLE #-} Show a => Show (SpreadSheet a) where
  show = showSpreadSheet

showSpreadSheet :: Show a => SpreadSheet a -> String
showSpreadSheet Empty = "Empty"
showSpreadSheet (Mk tl br s) = show (Range tl br) ++ "\n" ++ show s

{- Propiedades derivadas -}


-- | Indica el número de columnas que tiene una hoja de cálculo.
width :: SpreadSheet a -> Maybe Natural
width Empty = Nothing
width (Mk (x1, _) (x2, _) _) = Just (x2 - x1)

-- | Indica el número de filas que tiene una hoja de cálculo.
height :: SpreadSheet a -> Maybe Natural
height Empty = Nothing
height (Mk (_, y1) (_, y2) _) = Just (y2 - y1)

-- | Indica el rango en el que están contenidos los valores de la hoja
-- de cálculo.
limits :: SpreadSheet a -> Maybe Range
limits Empty = Nothing
limits (Mk tl br _) = Just $ Range tl br

-- | Enumera las columnas de la hoja de cálculo.
--
-- >>> columns empty
-- []
--
-- >>> columns $ fromList [((5,3), True), ((10,6), True)]
-- [5,6,7,8,9,10]
columns :: SpreadSheet a -> [Natural]
columns Empty = []
columns (Mk (tc, _) (bc, _) _) = [tc..bc]

-- | Enumera las filas de la hoja de cálculo.
--
-- >>> rows empty
-- []
--
-- >>> rows $ fromList [((5,3), True), ((10,6), True)]
-- [3,4,5,6]
rows :: SpreadSheet a -> [Natural]
rows Empty = []
rows (Mk (_, tr) (_, br) _) = [tr..br]

{- Constructures -}

-- | Hoja de cálculo vacía.
--
-- >>> empty == fromList []
-- True
empty :: SpreadSheet a
empty = Empty

-- | A partir de un solo valor, se genera la hoja que en la posición
-- (1,1) tiene dicho valor.
--
-- >>> singleton True
-- Range (1,1) (1,1)
-- fromList [((1,1), True)]
--
singleton :: a -> SpreadSheet a
singleton = singletonPos (1,1)

-- | Generar una hoja con un valor en una posición.
--
-- >>> singletonPos (1,5) True
-- Range (1,5) (1,5)
-- fromList [((1,5),True)]
--
singletonPos :: Pos -> a -> SpreadSheet a
singletonPos p v = fromList [(p,v)]

-- | De una lista de pares posición-valor se genera la hoja
-- correspondiente. En el caso de tener posiciones repetidas,
-- permanecerá la que tenga un índice mayor.
--
-- >>> fromList [((1,1), 0), ((1,2), 1), ((1,1), 5)]
-- Range (1,1) (1,2)
-- fromList [((1,1),5),((1,2),3)]
--
fromList :: [(Pos, a)] -> SpreadSheet a
fromList = fromDict . Map.fromList


-- | Construcción de forma equivalente a 'fromList', salvo que se usa un
-- diccionario de la librearía 'Data.Map.Map'.
--
-- Destinado a construir la hoja de cálculo aprovechando las funciones
-- optimizadas para diccionarios. Las precondiciones de las funciones
-- de construcción de diccionarios no se comprueban, ya que forman
-- parte de otro módulo.
--
-- >>> let cells = zipWith (,) [(x,y) | x <- [1..9], y <- [1..9]] [1..]
-- >>> Map.valid $ Map.fromAscList cells
-- True
-- >>> Map.valid $ Map.fromAscList (reverse cells)
-- False
-- >>> fromDict $ Map.fromAscList cells
-- Range (1,1) (9,9)
-- fromList [((1,1),1),((1,2),2),((1,3),3),((1,4),...
fromDict :: Map.Map Pos a -> SpreadSheet a
fromDict s
  | Map.null s = Empty
  | otherwise = fromDict' init end s
  where init = bimap min (min . Map.mapKeys swap) (s,s)
        end  = bimap max (max . Map.mapKeys swap) (s,s)
        min = fst . fst . Map.findMin
        max = fst . fst . Map.findMax

fromDict' :: Pos -> Pos -> Map.Map Pos a -> SpreadSheet a
fromDict' = Mk


-- | Transforma una hoja de cálculo a una lista que contiene sus
-- valores.
--
-- >>> toListValues empty == []
-- True
--
-- La función 'Data.Foldable.toList' aplicada a esta esctructura de
-- datos utiliza esta función.
toListValues :: SpreadSheet a -> [a]
toListValues Empty = []
toListValues (Mk _ _ s) = map snd $ Map.toList s


-- | Transforma una hoja de cálculo a una lista que contiene sus
-- valores junto a las posiciones en las que están.
toListPosValues :: SpreadSheet a -> [(Pos, a)]
toListPosValues Empty = []
toListPosValues (Mk _ _ s) = Map.toList s

{- Modificación -}

-- Probablemente se añada tambiém para hacer a <> a como resultado

-- | Combina dos hojas de cálculo. En el caso de que dos celdas
-- contengan datos en cada una de las dos hojas, dejará el dato de la
-- segunda.
--
-- También se puede usar el operador @(<>)@ de la clase 'Semigroup'.
union :: SpreadSheet a -> SpreadSheet a -> SpreadSheet a
union Empty x = x
union x Empty = x
union (Mk tl1 br1 s1) (Mk tl2 br2 s2) = Mk tl br s
  where tl = (min (fst tl1) (fst tl2), min (snd tl1) (snd tl2))
        br = (max (fst br1) (fst br2), max (snd br1) (snd br2))
        s  = flip Map.union s1 s2

-- | Genera una hoja de cálculo nueva con la intersección de las
-- celdas, dejando los valores de la primera hoja.
intersection :: SpreadSheet a -> SpreadSheet a -> SpreadSheet a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection s s'    = fromDict $ Map.intersection (mp s) (mp s')

-- | Operador para la intersección. Ver 'intersection'.
(</>) :: SpreadSheet a -> SpreadSheet a -> SpreadSheet a
(</>) = intersection

-- | Inserta en una posición un valor. Este valor se puede generar
-- según una función sobre una hoja de cálculo.
--
-- Esta función permite la composición de sucesivas inserciones.
--
-- >>> empty & put (1,1) (const True) & put (1,2) (const False)
-- Range (1,1) (1,2)
-- fromList [((1,1),True),((1,2),False)]
put :: Pos -> (SpreadSheet a -> a) -> SpreadSheet a -> SpreadSheet a
put pos f          Empty = fromDict $ Map.singleton pos (f Empty)
put pos f s@(Mk tl tr m) = fromDict $ Map.insert pos (f s) m


{- Seleccionar un rango de una SpreadSheet

Permite definir rangos fijos, como por ejemplo:

a3a4 = range Range (1,3) (1,4) :: Spreadsheet -> Spreadsheet --

-}


-- | Obtiene el valor de una posición ('Pos') de una hoja de cálculo.
--
-- Si la celda contiene un dato, devuelve el valor como ('Just' @valor@). Si no existe, devuelve 'Nothing'.
--
-- >>> get (1,1) $ fromList [((1,1), "valor")]
-- Just "valor"
--
-- >>> get (2,2) $ fromList [((1,1), "valor")]
-- Nothing
--
get :: Pos -> SpreadSheet a -> Maybe a
get _      Empty = Nothing
get p (Mk _ _ s) = Map.lookup p s

-- | Genera una hoja de cálculo a partir de una dada según una función
-- discriminante sobre las posiciónes.
--
-- En el siguiente ejemplo se eligen por ejemplo las celdas presentes en la diagonal.
--
-- >>> select (\(col,row) -> col == row) $ fromList [((1,1), "a"), ((2,1), "b"), ((1,2), "c"), ((2,2), "d")]
-- Range (1,1) (2,2)
-- fromList [((1,1),"a"),((2,2),"d")]
select :: (Pos -> Bool) -> SpreadSheet a -> SpreadSheet a
select _ Empty = Empty
select f s     = fromDict . fst . Map.partitionWithKey (\k _ -> f k) $ mp s


-- | Dando un rango y una hoja de cálculo, genera una hoja de cálculo
-- con las celdas en dicho rango.
--
-- >>> let cells = zipWith (,) [(x,y) | x <- [1..9], y <- [1..9]] [1..]
-- >>> range (Range (8,7) (9,9)) $ fromList cells
-- Range (8,7) (9,9)
-- fromList [((8,7),70),((8,8),71),((8,9),72),((9,7),79),((9,8),80),((9,9),81)]
selectRange :: Range -> SpreadSheet a -> SpreadSheet a
selectRange (Range (ic, ir) (fc, fr)) s
  | ic >= 0 && ir >= 0 && ic <= fc && ir <= fr = go
  | otherwise = Empty
  where go = select (\(c,r) -> ic <= c && ir <= r && fc >= c && fr >= r) s


-- | Selecciona las celdas con datos de una hoja de cálculo para una
-- columna dada.
--
-- >>> let cells = zipWith (,) [(x,y) | x <- [1..5], y <- [1..5]] [1..]
-- >>> column 2 $ fromList cells
-- Range (2,1) (2,5)
-- fromList [((2,1),6),((2,2),7),((2,3),8),((2,4),9),((2,5),10)]
column :: Natural -> SpreadSheet a -> SpreadSheet a
column n = select (\(c,r) -> c == n)


-- | Selecciona las celdas con datos de una hoja de cálculo para una
-- fila dada.
--
-- >>> let cells = zipWith (,) [(x,y) | x <- [1..5], y <- [1..5]] [1..]
-- >>> row 3 $ fromList cells
-- Range (1,3) (5,3)
-- fromList [((1,3),3),((2,3),8),((3,3),13),((4,3),18),((5,3),23)]
row :: Natural -> SpreadSheet a -> SpreadSheet a
row n = select (\(c,r) -> r == n)

--

-- | La función 'mapMaybe' está orientada a la selección de valores de
-- un tipo que puedan ser transformados a otro. Tomando una hoja de
-- cálculo se obtiene una nueva, con un tipo distinto, con solo
-- aquellos valores que han podido ser extraidos.
--
-- Esta función tiene su ejemplo en el conjunto de funciónes
-- 'Data.SpreadSheet.Cell.extractDouble',
-- 'Data.SpreadSheet.Cell.extractString' o
-- 'Data.SpreadSheet.Cell.extractDay' del módulo 'Data.SpreadSheet.Cell.Cell'.
--
-- >>> let days = fromList [((1,1), "2018-12-23"), ((1,2),"-"), ((1,3),"2018-12-25")] :: SpreadSheet String
-- >>> let parseDay = (\x -> parseTimeM True defaultTimeLocale "%Y-%-m-%-d" x) :: String -> Maybe Day
-- >>> :t mapMaybe parseDay days
-- extract p days :: SpreadSheet Day
-- >>> mapMaybe parseDay days
-- Range (1,1) (1,3)
-- fromList [((1,1),2018-12-23),((1,3),2018-12-25)]
mapMaybe :: (a -> Maybe b) -> SpreadSheet a -> SpreadSheet b
mapMaybe f Empty = Empty
mapMaybe f s = fromDict (Map.mapMaybe f (mp s))

-- zipWith :: (a -> b -> c) -> SpreadSheet a -> SpreadSheet b -> SpreadSheet c
