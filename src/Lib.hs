module Lib
  ( SpreadSheet
  , Range
  , Pos
  , Celldable
  , blank
  , fromList
  , extract
  , range
  , row
  , column
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Matrix as Mat
import Data.Bifunctor (bimap)
import Data.Tuple (swap)

type Pos = (Int, Int)

data Range = Range Pos Pos deriving Show

data SpreadSheet a = Mk { topl :: Pos  -- Top Left Position
                        , botr :: Pos  -- Bottom Right Position
                        , mp   :: Map.Map Pos a }

instance Functor SpreadSheet where
  fmap f (Mk tl br s) = Mk tl br (Map.map f s)

instance (Show a, Celldable a) => Show (SpreadSheet a) where
  show = pretty

class Show a => Celldable a where
  blank :: a

pretty :: Celldable a => SpreadSheet a -> String
pretty s = Mat.prettyMatrix mx
  where ss = fmap show s :: SpreadSheet String
        lp = (,) <$> [(fst $ topl s)..(fst $ botr s)] <*> [(snd $ topl s)..(snd $ botr s)] :: [Pos]
        ls = map (\k -> Map.findWithDefault blank k (mp s)) lp
        mx = Mat.fromList (fst $ botr s) (snd $ botr s) ls


fromList :: [(Pos, a)] -> SpreadSheet a
fromList = fromDict . Map.fromList

fromDict :: Map.Map Pos a -> SpreadSheet a
fromDict s = fromDict' init end s
  where init = bimap min (min . (Map.mapKeys swap)) (s,s)
        end  = bimap max (max . (Map.mapKeys swap)) (s,s)
        min = fst . fst . Map.findMin
        max = fst . fst . Map.findMax

fromDict' :: Pos -> Pos -> Map.Map Pos a -> SpreadSheet a
fromDict' init end s = Mk init end s
-- type HSpreadSheet a = Map.Map Pos a

{- Seleccionar un rango de una SpreadSheet

Permite definir rangos fijos, como por ejemplo:

a3a4 = range Range (1,3) (1,4) :: Spreadsheet -> Spreadsheet

-}

get :: (Pos -> Bool) -> SpreadSheet a -> SpreadSheet a
get f s = fromDict . fst . (Map.partitionWithKey (\a _ -> f a)) $ mp s

range :: Range -> SpreadSheet a -> SpreadSheet a
range (Range (ic, ir) (fc, fr)) s
  | ic >= 0 && ir >= 0 && ic <= fc && ir <= fr = go
  | otherwise = fromDict' (0,0) (0,0) Map.empty
  where go = get (\(c,r) -> ic <= c && ir <= r && fc >= c && fr >= r) s

column :: Int -> SpreadSheet a -> SpreadSheet a
column n = get (\(c,r) -> r == n) -- WTF a las 2am

row :: Int -> SpreadSheet a -> SpreadSheet a
row n = get (\(c,r) -> c == n) -- WTF a las 2am

--

extract :: (a -> Maybe b) -> SpreadSheet a -> SpreadSheet b
extract f s = fromDict (Map.mapMaybe f (mp s))
