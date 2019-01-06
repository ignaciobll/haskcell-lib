{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SpreadSheetSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.SpreadSheet
import Data.SpreadSheet.Internal.Gen
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function hiding (apply)
import qualified Test.QuickCheck.Function as TQF (apply)

import Test.Hspec.Checkers
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Numeric.Natural (Natural(..))
import Data.List (nubBy, sort, isSubsequenceOf)
import Data.Tuple (swap)
import Control.Applicative (liftA2, liftA3)
import qualified Data.Map.Strict as Map
import Data.Ix
import Data.Foldable (toList)

instance Eq a => EqProp (SpreadSheet a) where
  (=-=) = eq

spec :: Spec
spec = do
  -- | Tomados la expecificación de los ejemplos de 'Data.SpreadSheet.SpreadSheet'
  let s1 = [((5,3), True), ((10,6), True)]

  describe "columns" $ do
    it "returns 'empty list' when 'empty spreadsheet'" $ do
      columns empty `shouldBe` []

    it "returns list of columns index" $ do
      columns (fromList s1) `shouldBe` [5,6,7,8,9,10]

  describe "rows" $ do
    it "returns 'empty list' when 'empty spreadsheet'" $ do
      rows empty `shouldBe` []

    it "returns list of rows index" $ do
      rows (fromList s1) `shouldBe` [3,4,5,6]

  describe "empty" $ do
    context "with type annotations" $ do
      it "equals to 'from empty list'" $ do
        empty `shouldBe` (fromList [] :: SpreadSheet ())

  describe "singleton" $ do
    it "equals to value at (1,1)" $ do
      singleton True `shouldBe` fromList [((1,1), True)]

  describe "singletonPos" $ do
    it "equals to value at point" $ do
      singletonPos (1,5) True `shouldBe` fromList [((1,5),True)]

  describe "fromList" $ do
    context "without duplicates" $ do
      prop "spreadsheet has same numbers of elements as list" $
        forAll uniqueList same_length_spreadsheet

    context "with duplicates" $ do
      prop "should have the same length as the 'nub list'" $
        forAll (arbitrary :: Gen [(Pos, ())]) $ \l ->
        length (nubByIndex l) == length (fromList l)

  describe "toListValues" $ do
    it "returns 'empty' when has 'empty spreadsheet'" $ do
      toListValues (empty :: SpreadSheet ()) `shouldBe` []

  describe "union" $ do
    testBatch (monoid (undefined :: SpreadSheet Int))

  describe "should satisfy Functor Laws" $ do
    shouldSatisfyFunctorLaws (undefined :: SpreadSheet Int)

  describe "width" $ do
    context "being an empty spreadsheet" $ do
      it "should be 'Nothing'" $ do
        width empty `shouldBe` Nothing

    context "not being an empty spreadsheet" $ do
      prop "should return number of columns in the range" $ do
        width_prop

  describe "height" $ do
    context "beign an empty spreadsheet" $ do
      it "should be 'Nothing'" $ do
        height empty `shouldBe` Nothing

    context "not being an empty spreadsheet" $ do
      prop "should return number of rows in the range" $ do
        height_prop

  describe "limits" $ do
    context "beign an empty spreadsheet" $ do
      it "should be 'Nothing'" $ do
        limits empty `shouldBe` Nothing

    context "not being an empty spreadsheet" $ do
      prop "should return the range well bounded" $ do
        limits_prop

  describe "fromDict" $ do
    context "with empty dict" $ do
      it "should create an empty spreadsheet" $ do
        fromDict (Map.empty :: Map.Map Pos ()) `shouldBe` empty

    context "with valid dicts" $ do
      it "creates a well bounded spreadsheet" $ do
        width_prop .&&. height_prop .&&. limits_prop

  describe "rows" $ do
    prop "should give a continuous list well bounded" $ do
      forAll (pos_list :: Gen [(Pos, ())]) rows_prop

  describe "columns" $ do
    prop "should give a continuous list well bounded" $ do
      forAll (pos_list :: Gen [(Pos, ())]) columns_prop

  describe "toListValues" $ do
    prop "list must containg all values" $ do
      forAll (pos_list :: Gen [(Pos, Int)]) toList_prop

  describe "intersection" $ do
    prop "should satisfy associativity laws" $ do
      isAssoc (intersection :: SpreadSheet Int -> SpreadSheet Int -> SpreadSheet Int)

    prop "should be idempotent" $ do
      idempotent2 (intersection :: SpreadSheet Int -> SpreadSheet Int -> SpreadSheet Int)

    context "using operator (</>)" $ do
      prop "should satisfy associativity laws" $ do
        isAssoc ((</>) :: SpreadSheet Int -> SpreadSheet Int -> SpreadSheet Int)

      prop "should be idempotent" $ do
        idempotent2 ((</>) :: SpreadSheet Int -> SpreadSheet Int -> SpreadSheet Int)

  describe "get" $ do
    prop "should retrieve value if exists in creational list" $ do
      get_prop

  describe "put" $ do
    it "after it, value should be able to be retrieved with 'get'" $ do
      put_val_prop .&&. put_fun_prop

  describe "select" $ do
    prop "gets filtered values" $ do
      select_rel :: Fun Pos Bool -> SpreadSheet Int -> Bool

  describe "selectRange" $ do
    prop "gets the same amount of values a in list" $ do
      selectRange_rel :: Range -> SpreadSheet () -> Bool

  describe "column" $ do
    prop "gets all values in column n" $ do
      column_rel :: Natural -> SpreadSheet () -> Bool

  describe "row" $ do
    prop "gets all values in row n" $ do
      row_rel :: Natural -> SpreadSheet () -> Bool

  describe "mapMaybe" $ do
    prop "get only Just values" $ do
      mapMaybe_rel :: Fun Int (Maybe Int) -> SpreadSheet Int -> Bool

  describe "show" $ do
    prop "should have all values listed" $ do
      prop_show

  describe "toList" $  do
    prop "should create a list with pos and values" $ do
      (\s -> toList s == (toListValues s)) :: SpreadSheet Int -> Bool


-- uniqueList :: (Arbitrary a, Show a) => Gen [(Pos, a)]
uniqueList :: Gen [(Pos, ())]
uniqueList = arbitrary `suchThat` (\l -> length l == (length (nubByIndex l)))

nubByIndex = nubBy $ \(i, _) (j, _) -> i == j

same_length_spreadsheet :: [(Pos, a)] -> Bool
same_length_spreadsheet ul = length ul == length (fromList ul)

--

shouldSatisfyFunctorLaws :: (Eq (f a), Show (f a), Functor f, Arbitrary (f a)) => f a -> Spec
shouldSatisfyFunctorLaws t = do
  describe "identity" $ do
    it "returns the value unchanged" $ property $ \x ->
      fmap id x == x `asTypeOf` t

  describe "composition" $ do
    it "two functors one after another is the same as their composition" $ property $
      (functorCompProp :: SpreadSheet Int -> Fun Int Int -> Fun Int Int -> Bool)

-- | https://austinrochford.com/posts/2014-05-27-quickcheck-laws.html
functorCompProp :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompProp x (TQF.apply -> f) (TQF.apply -> g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

--

pos_list_at_least :: (Show a, Arbitrary a) => Int -> Gen [(Pos, a)]
pos_list_at_least n = sized $ \s ->
  do k <- choose (n, n+s)
     vectorOf k arbitrary

pos_list :: (Show a, Arbitrary a) => Gen [(Pos, a)]
pos_list = pos_list_at_least 0

val_pos_pos_list :: (Show a, Arbitrary a) => Gen (a, Pos, [(Pos, a)])
val_pos_pos_list = liftA3 (\x y z -> (x,y,z)) arbitrary arbitrary pos_list

pos_and_pos_list :: (Show a, Arbitrary a) => Gen (Pos, [(Pos, a)])
pos_and_pos_list = liftA2 (,) arbitrary pos_list

non_empty_pos_list :: (Show a, Arbitrary a) => Gen [(Pos, a)]
non_empty_pos_list = pos_list_at_least 1


minCol = fst . foldr1 min . map fst
maxCol = fst . foldr1 max . map fst
minRow = fst . foldr1 min . map (swap . fst)
maxRow = fst . foldr1 max . map (swap . fst)


width_prop = forAll (pos_list :: Gen [(Pos, ())]) width_rel
width_rel :: [(Pos, a)] -> Bool
width_rel [] = Nothing == (width $ fromList [])
width_rel pl = _minCol >= 0 && _maxCol >= _minCol
                && Just (_maxCol - _minCol) == width (fromList pl)
  where _minCol = minCol pl
        _maxCol = maxCol pl


height_prop = forAll (pos_list :: Gen [(Pos, ())]) height_rel
height_rel :: [(Pos, a)] -> Bool
height_rel [] = Nothing == (height $ fromList [])
height_rel pl = _minRow >= 0 && _maxRow >= _minRow
                 && Just (_maxRow - _minRow) == height (fromList pl)
  where _minRow = minRow pl
        _maxRow = maxRow pl

limits_prop = forAll (pos_list :: Gen [(Pos, ())]) limits_rel
limits_rel :: [(Pos, a)] -> Bool
limits_rel [] = Nothing == (limits $ fromList [])
limits_rel pl = _minCol >= 0 && _minRow >= 0
                 && _maxCol >= _minCol && _maxRow >= _minRow
                 && Just (Range (_minCol, _minRow) (_maxCol, _maxRow)) == limits (fromList pl)
  where _minCol = minCol pl
        _maxCol = maxCol pl
        _minRow = minRow pl
        _maxRow = maxRow pl

rows_prop :: [(Pos, a)] -> Bool
rows_prop [] = [] == (rows $ fromList [])
rows_prop pl@[((_col, row), _)] = [row] == (rows $ fromList pl)
rows_prop pl = (1 + _maxRow - _minRow) == (fromIntegral . length . rows $ s)
               && [_minRow.._maxRow] == rows s
  where _minRow = minRow pl
        _maxRow = maxRow pl
        s = fromList pl

columns_prop :: [(Pos, a)] -> Bool
columns_prop [] = [] == (columns $ fromList [])
columns_prop pl@[((col, _row), _)] = [col] == (columns $ fromList pl)
columns_prop pl = (1 + _maxCol - _minCol) == (fromIntegral . length . columns $ s)
               && [_minCol.._maxCol] == columns s
  where _minCol = minCol pl
        _maxCol = maxCol pl
        s = fromList pl


-- | 'nubByIndex' compara por posiciones y deja el valor del elemento
-- repetido. Sin embargo, 'Data.SpradSheet.fromList' deja el último
-- valor. Se ajusta mediante 'reverse'.
--
-- Se restringe para datos que puedan tener un orden, para poder
-- comprobar un a uno que todos los datos están presentens.
toList_prop :: Ord a => [(Pos, a)] -> Bool
toList_prop pl = (==)
  (sort $ map snd (nubByIndex (reverse pl)))
  (sort . toListValues . fromList $ pl)

get_prop = forAll (pos_and_pos_list :: Gen (Pos, [(Pos, Int)])) get_rel
get_rel :: (Eq a, Ord a) => (Pos, [(Pos, a)]) -> Bool
get_rel (pos, pl)
  | pos `elem` (map fst pl) = get pos sheet == lookup pos (reverse pl)
  | otherwise = get pos sheet == Nothing
  where sheet = fromList pl


put_val_prop = forAll (val_pos_pos_list :: Gen (Int, Pos, [(Pos, Int)])) put_val_rel
put_val_rel :: Eq a => (a, Pos, [(Pos, a)]) -> Bool
put_val_rel (val, pos, pl) = (get pos $ put pos (const val) s) == Just val
  where s = fromList pl

put_fun_prop = property (put_fun_rel :: Fun (SpreadSheet Int) Int ->  Pos -> SpreadSheet Int -> Bool)
put_fun_rel :: Eq a => Fun (SpreadSheet a) a -> Pos -> SpreadSheet a -> Bool
put_fun_rel (TQF.apply -> f) pos s = (get pos $ put pos f s) == Just (f s)

-- functorCompProp :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
-- functorCompProp x (TQF.apply -> f) (TQF.apply -> g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

select_rel :: Eq a => Fun Pos Bool -> SpreadSheet a -> Bool
select_rel (TQF.apply -> f) s = select f s == (fromList $ (selectList f) (toListPosValues s))
  where selectList f = filter (f . fst)


-- filter (\pos -> pos `elem` rsel) (map fst $ toListPosValues s)
selectRange_rel :: Eq a => Range -> SpreadSheet a -> Bool
selectRange_rel r@(Range p@(ic, ir) p'@(fc, fr)) s
  | s == empty = Nothing == limits'
  | ic <= fc && ir <= fr = length (filterInRange p p' s) == (length $ selectRange r s)
  | otherwise = Nothing == limits'
  where limits' = (limits $ selectRange r s)
        filterInRange p p' s = filter (inRange (p, p')) (map fst $ toListPosValues s)


column_rel :: Eq a => Natural -> SpreadSheet a -> Bool
column_rel n s
  | s == empty = empty == column n s
  | n `elem` [ic..fc]  = (sort $ filterInCol n s) == (sort . map fst . toListPosValues $ column n s)
  | otherwise = empty == column n s
  where (ic,fc) = case limits s of
          Just (Range (ic,_ir) (fc,_fr)) -> (ic,fc)
          Nothing -> (0,0) -- impossible branch
        filterInCol n s = filter (\(c,_r) -> c == n) (toListOfPos s)
        toListOfPos = (map fst) . toListPosValues

row_rel :: Eq a => Natural -> SpreadSheet a -> Bool
row_rel n s
  | s == empty = empty == row n s
  | n `elem` [ir..fr]  = (sort . map swap $ filterInCol n s) == (sort . map (swap . fst) . toListPosValues $ row n s)
  | otherwise = empty == row n s
  where (ir,fr) = case limits s of
          Just (Range (_ic,ir) (_fc,fr)) -> (ir,fr)
          Nothing -> (0,0) -- impossible branch
        filterInCol n s = filter (\(_c,r) -> r == n) (toListOfPos s)
        toListOfPos = (map fst) . toListPosValues


mapMaybe_rel :: (Eq b, Ord b) => Fun a (Maybe b) -> SpreadSheet a -> Bool
mapMaybe_rel (TQF.apply -> f) s = (sort $ mapMaybeList s) == (sort . toListPosValues $ mapMaybe f s)
  where mapMaybeList s = map (\(pos, Just val) -> (pos, val))
                         . filter (\(p, val) -> val /= Nothing)
                         . map (\(p,val) -> (p, f val)) $
                         toListPosValues s

prop_show :: SpreadSheet Int -> Property
prop_show s
  | s == empty = property $ (show s) == "Empty"
  | otherwise = property $ all (\value -> any id $ map (\v -> value `isSubsequenceOf` v) visual) values
  where values = map show $ toListPosValues s
        visual = words $ show s
