module Data.SpreadSheet.Internal.Pretty
  ( Celldable(..)
  , prettyShowSpreadSheet
  ) where

import Data.SpreadSheet
import Data.SpreadSheet (SpreadSheet(..))
import qualified Data.Matrix as Mat
import qualified Data.Map.Strict as Map

-- | Ante la uniformidad de las hojas de cálculo ('SpreadSheet' @a@,
-- 'SpreadSheet' 'Double') algunas de estas celdas pueden no tener
-- valor. Esto no supone un problema para trabajar con ellas, salvo
-- cuando queremos representar todo el conjuto de celdas que engloba
-- el rango de una hoja.
--
-- Se podría realizar una visualización de solo las celdas con
-- valores, para la que no haría falta una implementación de
class Celldable a where
  blank :: a

-- | Se condiciona a que sea 'Celldable' para que aquellas celdas
-- vacías tengan representación visual válida a elección de la
-- implementación.
--
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
prettyShowSpreadSheet :: (Show a, Celldable a) => SpreadSheet a -> String
prettyShowSpreadSheet Empty = "Empty"
prettyShowSpreadSheet s = (show $ Range (topl s) (botr s)) ++ "\n" ++ Mat.prettyMatrix mx
  where lp = (,) <$> [(fst $ topl s)..(fst $ botr s)] <*> [(snd $ topl s)..(snd $ botr s)] :: [Pos]
        ls = map (\k -> Map.findWithDefault blank k (mp s)) lp
        nrows = (1 + (fst $ botr s) - (fst $ topl s))
        ncols = (1 + (snd $ botr s) - (snd $ topl s))
        mx = Mat.transpose $ Mat.fromList nrows ncols ls
