-- | Módulo interno que define una forma más visual de representar una
-- hoja de cálculo de forma textual.
--
-- Ofrece una alternativa a la función 'show' para redefinir la
-- visualización de una hoja de cáluclo
-- ('Data.SpreadSheet.SpreadSheet').
--
-- Este módulo es *unsafe* al aprovechar la función 'show' de
-- 'Data.Matrix.Matrix'.
--
-- Para poder usar de forma efectiva este módulo, se tendrá que
-- definir una implementación de la clase 'Celldable', indicando la
-- representación del valor vacío.
--
-- Ante la uniformidad de las hojas de cálculo ('SpreadSheet' @a@,
-- 'SpreadSheet' 'Double') algunas de estas celdas pueden no tener
-- valor. Esto no supone un problema para trabajar con ellas, salvo
-- cuando queremos representar todo el conjuto de celdas que engloba
-- el rango de una hoja.
--
-- == Ejemplo de visualización
-- Para visualizar por ejemplo una hoja de cálculo de números
-- ('SpreadSheet' 'Double') no tenemos un valor 'Double' para las
-- celdas vacías. La visualización por tanto, no sería posible. Si a
-- pesar de esto queremos visualizar dicha hoja de cálculo, se deberá
-- indicar con qué valor 'Double' se representará la celda vacía. Este
-- valor se indica instancianddo la clase 'Celldable'.
--
-- Tomamos por ejemplo el valor @0@:
--
-- @
-- instance Celldable Double where
--   blank = 0
-- @
--
-- /Esto no quiere decir que las celdas tengan valor 0, sino que/
-- /se visualizan como 0./
--
-- Si se quiere usar esta función para la visualización por defecto,
-- se recomienda que tras instanciar 'Celldable' se realice la
-- sobreescritura de la clase 'Show':
--
-- @
-- instance {-\# OVERLAPS -\#} Show (SpreadSheet Double) where
--   show = prettyShowSpreadSheet
-- @
--
-- >>> fromList [((1,1), 1),((2,3), 3)] :: SpreadSheet Double
-- Range (1,1) (2,3)
-- ┌          ┐
-- │ 1.0  0.0 │
-- │ 0.0  0.0 │
-- │ 0.0  3.0 │
-- └          ┘
module Data.SpreadSheet.Internal.Pretty
  ( Celldable(..)
  , prettyShowSpreadSheet
  ) where

import Data.SpreadSheet
import qualified Data.Matrix as Mat
import qualified Data.Map.Strict as Map

-- | Clase para indicar el valor con el que se representarán las
-- celdas vacías.
class Celldable a where
  blank :: a

-- | Función alternativa para representar una hoja de cálculo.
prettyShowSpreadSheet :: (Show a, Celldable a) => SpreadSheet a -> String
prettyShowSpreadSheet Empty = "Empty"
prettyShowSpreadSheet s = show (Range (topl s) (botr s)) ++ "\n" ++ Mat.prettyMatrix mx
  where lp = (,) <$> [(fst $ topl s)..(fst $ botr s)] <*> [(snd $ topl s)..(snd $ botr s)] :: [Pos]
        ls = map (\k -> Map.findWithDefault blank k (mp s)) lp
        nrows = fromIntegral $ 1 + fst  (botr s) - fst (topl s)
        ncols = fromIntegral $ 1 + snd  (botr s) - snd (topl s)
        mx = Mat.transpose $ Mat.fromList nrows ncols ls
