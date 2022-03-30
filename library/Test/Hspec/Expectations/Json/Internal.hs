{-# LANGUAGE CPP #-}

-- | Internal building-blocks for JSON 'Value' expectations
module Test.Hspec.Expectations.Json.Internal
  (
  -- * Pretty diff
    assertBoolWithDiff

  -- * Pruning 'Object's
  , Superset(..)
  , Subset(..)
  , pruneJson

  -- * Sorting 'Array's
  , Sortable(..)
  , sortJsonArrays
  , vectorSortOn

  -- * Dealing with 'Scientific'
  , normalizeScientific
  )
where

import Prelude


import Data.Aeson
#if MIN_VERSION_Diff(0,4,0)
import Data.Algorithm.Diff (PolyDiff(..), getDiff)
#else
import Data.Algorithm.Diff (Diff(..), getDiff)
#endif
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif
import Data.List (sortOn)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack (HasCallStack)
import qualified Test.HUnit as HUnit

-- So we can call HashMap KeyMap in older aeson
{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}

assertBoolWithDiff :: HasCallStack => Bool -> Text -> Text -> IO ()
assertBoolWithDiff asserting expected got =
  flip HUnit.assertBool asserting . unlines . map addSign $ getDiff
    (lines (T.unpack expected))
    (lines (T.unpack got))
 where
  addSign = \case
    Both _ s -> "   " ++ s
    First s -> "---" ++ s
    Second s -> "+++" ++ s

newtype Superset = Superset Value

newtype Subset = Subset Value

-- | Recursively remove items in the 'Superset' value not present in 'Subset'
pruneJson :: Superset -> Subset -> Value
pruneJson (Superset sup) (Subset sub) = case (sup, sub) of
  (Object a, Object b) -> Object
    $ KeyMap.intersectionWith (\x y -> pruneJson (Superset x) (Subset y)) a b

  -- Pruning elements in Arrays is *extremely* tricky in that it interacts with
  -- both sorting and matching in what should be a function independent of those
  -- concerns. There are no good options here, so we make some concessions:
  --
  -- 1. It's expected you don't subset differently in different elements of the
  --    same list. If you have an assertion that needs this behavior, do it
  --    manually, please
  --
  -- 2. It's expected that sorting will be done after pruning, if you intend to
  --    match irrespective of extra keys or ordering (shouldMatchJson does this)
  --
  -- Therefore, we grab the first element from the Subset Array (if present) and
  -- prune all elements of the Superset Array using it. This ensures that
  -- different sorts or length in the Superset side are preserved, but we
  -- are still able to prune *before* the sorting required for matching, which
  -- is important.
  --
  -- Other options such as sort-before-prune, or pair-wise pruning (with align
  -- or zip) all correctly handle some cases but not all. And most importantly,
  -- the cases those options don't handle come out as confusing assertion
  -- failures.
  --
  (Array a, Array b) -> Array $ case b V.!? 0 of
    Nothing -> a
    Just y -> (\x -> pruneJson (Superset x) (Subset y)) <$> a

  (x, _) -> x

newtype Sortable = Sortable Value
  deriving newtype Eq

instance Ord Sortable where
  Sortable a `compare` Sortable b = case (a, b) of
    (String x, String y) -> x `compare` y
    (Number x, Number y) -> x `compare` y
    (Bool x, Bool y) -> x `compare` y
    (Null, Null) -> EQ -- forgive me
    (Array x, Array y) -> V.map Sortable x `compare` V.map Sortable y
    (Object x, Object y) ->
      (Sortable <$> x) `compare` (Sortable <$> y)
    (x, y) -> arbitraryRank x `compare` arbitraryRank y
   where
    arbitraryRank :: Value -> Int
    arbitraryRank = \case
      Object{} -> 5
      Array{} -> 4
      String{} -> 3
      Number{} -> 2
      Bool{} -> 1
      Null -> 0

sortJsonArrays :: Value -> Value
sortJsonArrays = \case
  Array v -> Array $ vectorSortOn Sortable $ sortJsonArrays <$> v
  Object hm -> Object $ sortJsonArrays <$> hm
  x@String{} -> x
  x@Number{} -> x
  x@Bool{} -> x
  x@Null{} -> x

vectorSortOn :: Ord b => (a -> b) -> Vector a -> Vector a
vectorSortOn f v = v V.// zip [0 ..] sorted
  where sorted = sortOn f $ V.toList v

-- | Normalize all 'Number' values to 'Double' precision
--
-- Internally, @1@ and @1.0@ are represented as different values of the
-- 'Scientific' data type. These will compare equally, but if there is some
-- /other/ difference that fails the assertion, they will render as a difference
-- in the message, confusing the reader.
--
-- This sends them through an 'id' function as 'Double', which will make either
-- print as @1.0@ consistently.
--
normalizeScientific :: Value -> Value
normalizeScientific = \case
  Object hm -> Object $ normalizeScientific <$> hm
  Array vs -> Array $ normalizeScientific <$> vs
  x@String{} -> x
  Number sci ->
    Number $ Scientific.fromFloatDigits @Double $ Scientific.toRealFloat sci
  x@Bool{} -> x
  x@Null -> x
