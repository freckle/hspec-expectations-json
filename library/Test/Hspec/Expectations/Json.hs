-- | Expectations on JSON 'Value's
--
-- Semantics:
--
-- +--------------------------+-------------------+-------------------+
-- | Assertion that fails on: | extra Object keys | wrong Array order +
-- +==========================+===================+===================+
-- | 'shouldBeJson'           | Yes               | Yes               |
-- +--------------------------+-------------------+-------------------+
-- | 'shouldBeUnorderedJson'  | Yes               | No                |
-- +--------------------------+-------------------+-------------------+
-- | 'shouldMatchJson'        | No                | No                |
-- +--------------------------+-------------------+-------------------+
-- | 'shouldMatchOrderedJson' | No                | Yes               |
-- +--------------------------+-------------------+-------------------+
module Test.Hspec.Expectations.Json
  ( shouldMatchJson
  , shouldBeJson
  , shouldBeJsonNormalized
  , Normalizer
  , defaultNormalizer
  , treatNullsAsMissing
  , ignoreArrayOrdering
  , subsetActualToExpected
  , expandHeterogenousArrays

    -- * Legacy API

    -- | Prefer to use shouldBeJsonNormalized with the appropriate 'Normalizer'
  , shouldBeUnorderedJson
  , shouldMatchOrderedJson

    -- * As predicates

    -- | These are only created when a specific need arises
  , matchesJson
  ) where

import Prelude

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor
import Data.Semigroup (Endo (..))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Stack
import Test.Hspec.Expectations.Json.Internal
  ( Subset (..)
  , Superset (..)
  , assertBoolWithDiff
  , filterNullFields
  , normalizeScientific
  , pruneJson
  , sortJsonArrays
  )
import qualified Test.Hspec.Expectations.Json.Internal as Internal

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
-- >>> import Control.Exception (handle)
-- >>> let printFailure (HUnitFailure _ r) = putStr $ formatFailureReason r
-- >>> let catchFailure f = handle printFailure $ f >> putStrLn "<passed>"

newtype Actual a = Actual a
  deriving (Functor)

newtype Expected a = Expected a
  deriving (Functor)

newtype Normalizer = Normalizer
  { normalize :: Endo (Actual Value, Expected Value)
  }
  deriving newtype (Semigroup, Monoid)

normalizeBoth :: (Value -> Value) -> Normalizer
normalizeBoth f = Normalizer $ Endo $ bimap (fmap f) (fmap f)

treatNullsAsMissing :: Normalizer
treatNullsAsMissing = normalizeBoth filterNullFields

ignoreArrayOrdering :: Normalizer
ignoreArrayOrdering = normalizeBoth sortJsonArrays

expandHeterogenousArrays :: Normalizer
expandHeterogenousArrays = normalizeBoth Internal.expandHeterogenousArrays

subsetActualToExpected :: Normalizer
subsetActualToExpected = Normalizer $ Endo go
 where
  go (Actual a, Expected b) =
    let a' = pruneJson (Superset a) (Subset b)
    in  (Actual a', Expected b)

defaultNormalizer :: Normalizer
defaultNormalizer =
  ignoreArrayOrdering <> subsetActualToExpected

shouldBeJsonNormalized :: HasCallStack => Normalizer -> Value -> Value -> IO ()
shouldBeJsonNormalized normalizer a b =
  unless (a == b) $
    assertBoolWithDiff (a' == b') (toText b) (toText a)
 where
  toText = toStrict . decodeUtf8 . encodePretty . normalizeScientific
  (Actual a', Expected b') = appEndo (normalize normalizer) (Actual a, Expected b)

-- | Compare two JSON values, with a useful diff
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": true, "b": false } |] `shouldBeJson`
--   [aesonQQ| { "a": true, "b": false } |]
-- :}
-- <passed>
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": true, "b": false } |] `shouldBeJson`
--   [aesonQQ| { "a": true, "b": true  } |]
-- :}
--    {
--        "a": true,
-- ---    "b": true
-- +++    "b": false
--    }
shouldBeJson :: HasCallStack => Value -> Value -> IO ()
shouldBeJson = shouldBeJsonNormalized mempty

infix 1 `shouldBeJson`

-- | 'shouldBeJson', ignoring Array ordering
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false } |] `shouldBeUnorderedJson`
--   [aesonQQ| { "a": [false, true], "b": false } |]
-- :}
-- <passed>
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldBeUnorderedJson`
--   [aesonQQ| { "a": [false, true], "b": true             } |]
-- :}
--    {
--        "a": [
--            false,
--            true
--        ],
-- ---    "b": true
-- +++    "b": false,
-- +++    "c": true
--    }
shouldBeUnorderedJson :: HasCallStack => Value -> Value -> IO ()
shouldBeUnorderedJson = shouldBeJsonNormalized ignoreArrayOrdering

infix 1 `shouldBeUnorderedJson`

-- | 'shouldBeJson', ignoring extra Object keys or Array ordering
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchJson`
--   [aesonQQ| { "a": [false, true], "b": false            } |]
-- :}
-- <passed>
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchJson`
--   [aesonQQ| { "a": [false, true], "b": true             } |]
-- :}
--    {
--        "a": [
--            false,
--            true
--        ],
-- ---    "b": true
-- +++    "b": false
--    }
shouldMatchJson :: HasCallStack => Value -> Value -> IO ()
shouldMatchJson = shouldBeJsonNormalized defaultNormalizer

infix 1 `shouldMatchJson`

-- | Compare JSON values with the same semantics as 'shouldMatchJson'
matchesJson :: Value -> Value -> Bool
matchesJson sup sub = sup == sub || sup' == sub'
 where
  (Actual sup', Expected sub') = appEndo (normalize defaultNormalizer) (Actual sup, Expected sub)

-- | 'shouldBeJson', ignoring extra Object keys
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchOrderedJson`
--   [aesonQQ| { "a": [true, false], "b": false            } |]
-- :}
-- <passed>
--
-- >>> :{
-- catchFailure $
--   [aesonQQ| { "a": [true, false], "b": false, "c": true } |] `shouldMatchOrderedJson`
--   [aesonQQ| { "a": [false, true], "b": true             } |]
-- :}
--    {
--        "a": [
-- ---        false,
-- ---        true
-- +++        true,
-- +++        false
--        ],
-- ---    "b": true
-- +++    "b": false
--    }
shouldMatchOrderedJson :: HasCallStack => Value -> Value -> IO ()
shouldMatchOrderedJson = shouldBeJsonNormalized subsetActualToExpected

infix 1 `shouldMatchOrderedJson`
