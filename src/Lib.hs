module Lib
  ( SpreadSheet
  , Range
  , Pos
  , Celldable
  , blank
  , fromList
  , fromDict
  , extract
  , range
  , row
  , column
  , topl
  , botr
  , mp
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Matrix as Mat
import Data.Bifunctor (bimap)
import Data.Tuple (swap)
-- import Control.Applicative (liftA2)

type Pos = (Int, Int)

data Range = Range Pos Pos deriving Show

data SpreadSheet a = Mk { topl :: Pos  -- Top Left Position
                        , botr :: Pos  -- Bottom Right Position
                        , mp   :: Map.Map Pos a } | Empty

instance Eq a => Eq (SpreadSheet a) where
  Empty == Empty = True
  (Mk tl1 br1 s1) == (Mk tl2 br2 s2) = (tl1 == tl2) && (br1 == br2) && (s1 == s2)
  _ == _ = False

instance Functor SpreadSheet where
  fmap f (Mk tl br s) = Mk tl br (Map.map f s)

instance Semigroup (SpreadSheet a) where
  (<>) = union

instance (Show a, Celldable a) => Show (SpreadSheet a) where
  show = pretty

instance Monoid (SpreadSheet a) where
  mempty = Empty

instance Foldable SpreadSheet where
  foldr _ z Empty  = z
  foldr f z (Mk _ _ s) = Map.foldr f z s

class Show a => Celldable a where
  blank :: a

pretty :: Celldable a => SpreadSheet a -> String
pretty Empty = "Empty"
pretty s = (show $ Range (topl s) (botr s)) ++ "\n" ++ Mat.prettyMatrix mx
  where lp = (,) <$> [(fst $ topl s)..(fst $ botr s)] <*> [(snd $ topl s)..(snd $ botr s)] :: [Pos]
        ls = map (\k -> Map.findWithDefault blank k (mp s)) lp
        nrows = (1 + (fst $ botr s) - (fst $ topl s))
        ncols = (1 + (snd $ botr s) - (snd $ topl s))
        mx = Mat.transpose $ Mat.fromList nrows ncols ls


fromList :: [(Pos, a)] -> SpreadSheet a
fromList = fromDict . Map.fromList

fromDict :: Map.Map Pos a -> SpreadSheet a
fromDict s
  | Map.null s = Empty
  | otherwise = fromDict' init end s
  where init = bimap min (min . (Map.mapKeys swap)) (s,s)
        end  = bimap max (max . (Map.mapKeys swap)) (s,s)
        min = fst . fst . Map.findMin
        max = fst . fst . Map.findMax

fromDict' :: Pos -> Pos -> Map.Map Pos a -> SpreadSheet a
fromDict' init end s = Mk init end s
-- type HSpreadSheet a = Map.Map Pos a


-- Probablemente se añada tambiém para hacer a <> a como resultado
union :: SpreadSheet a -> SpreadSheet a -> SpreadSheet a
union Empty x = x
union x Empty = x
union (Mk tl1 br1 s1) (Mk tl2 br2 s2) = Mk tl br s
  where tl = (min (fst tl1) (fst tl2), min (snd tl1) (snd tl2))
        br = (max (fst br1) (fst br2), max (snd br1) (snd br2))
        s  = flip Map.union s1 s2


{- Seleccionar un rango de una SpreadSheet

Permite definir rangos fijos, como por ejemplo:

a3a4 = range Range (1,3) (1,4) :: Spreadsheet -> Spreadsheet

-}

get :: (Pos -> Bool) -> SpreadSheet a -> SpreadSheet a
get _ Empty = Empty
get f s     = fromDict . fst . (Map.partitionWithKey (\k _ -> f k)) $ mp s

range :: Range -> SpreadSheet a -> SpreadSheet a
range (Range (ic, ir) (fc, fr)) s
  | ic >= 0 && ir >= 0 && ic <= fc && ir <= fr = go
  | otherwise = Empty -- fromDict' (0,0) (0,0) Map.empty
  where go = get (\(c,r) -> ic <= c && ir <= r && fc >= c && fr >= r) s

column :: Int -> SpreadSheet a -> SpreadSheet a
column n = get (\(c,r) -> c == n)

row :: Int -> SpreadSheet a -> SpreadSheet a
row n = get (\(c,r) -> r == n)

--

extract :: (a -> Maybe b) -> SpreadSheet a -> SpreadSheet b
extract f s = fromDict (Map.mapMaybe f (mp s))

--

-- zipWith :: (a -> b -> c) -> SpreadSheet a -> SpreadSheet b -> SpreadSheet c
