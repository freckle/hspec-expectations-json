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
  ( shouldBeJson
  , shouldBeUnorderedJson
  , shouldMatchJson
  , shouldMatchOrderedJson

    -- * As predicates

    -- | These are only created when a specific need arises
  , matchesJson
  ) where

import Prelude

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Stack
import Test.Hspec.Expectations.Json.Internal

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
-- >>> import Control.Exception (handle)
-- >>> let printFailure (HUnitFailure _ r) = putStr $ formatFailureReason r
-- >>> let catchFailure f = handle printFailure $ f >> putStrLn "<passed>"

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
shouldBeJson a b = assertBoolWithDiff (a == b) (toText b) (toText a)
 where
  toText = toStrict . decodeUtf8 . encodePretty . normalizeScientific

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
shouldBeUnorderedJson a b =
  unless (a == b) $ sortJsonArrays a `shouldBeJson` sortJsonArrays b

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
shouldMatchJson sup sub =
  unless (sup == sub) $
    sortJsonArrays (pruneJson (Superset sup) (Subset sub))
      `shouldBeJson` sortJsonArrays sub

infix 1 `shouldMatchJson`

-- | Compare JSON values with the same semantics as 'shouldMatchJson'
matchesJson :: Value -> Value -> Bool
matchesJson sup sub =
  sup
    == sub
    || sortJsonArrays (pruneJson (Superset sup) (Subset sub))
      == sortJsonArrays sub

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
shouldMatchOrderedJson sup sub =
  unless (sup == sub) $ pruneJson (Superset sup) (Subset sub) `shouldBeJson` sub

infix 1 `shouldMatchOrderedJson`
