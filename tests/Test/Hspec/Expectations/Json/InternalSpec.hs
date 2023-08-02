{-# LANGUAGE QuasiQuotes #-}

module Test.Hspec.Expectations.Json.InternalSpec
  ( spec
  )
where

import Prelude

import Data.Aeson.QQ
import Test.Hspec
import Test.Hspec.Expectations.Json (shouldBeJson)
import Test.Hspec.Expectations.Json.Internal

spec :: Spec
spec = do
  describe "pruneJson" $ do
    it "prunes object keys from Superset not present in Subset" $ do
      let
        sup = Superset [aesonQQ|{ "foo": "foo", "baz": "baz" }|]
        sub = Subset [aesonQQ|{ "foo": "bar" } |]

      pruneJson sup sub `shouldBeJson` [aesonQQ|{ "foo": "foo" }|]

    it "prunes object keys recursively Superset not present in Subset" $ do
      let
        sup =
          Superset
            [aesonQQ|
          { "foo": { "bar": "bar" , "baz": "baz" }
          , "bat": "bat"
          }
        |]

        sub =
          Subset
            [aesonQQ|
          { "foo": { "bar": "baz" }
          }
        |]

      pruneJson sup sub `shouldBeJson` [aesonQQ|{ "foo": { "bar": "bar" } }|]

    it "prunes objects within Arrays" $ do
      let
        sup =
          Superset
            [aesonQQ|
          [ { "foo": "bar", "quix": "cats" }
          , { "foo": "bats", "quix": "baz" }
          ]
        |]

        sub =
          Subset
            [aesonQQ|
          [ { "foo": "zap" }
          , { "foo": "zop" }
          ]
        |]

      pruneJson sup sub
        `shouldBeJson` [aesonQQ|
       [ { "foo": "bar" }
       , { "foo": "bats" }
       ]
      |]

    it "handles mismatching types" $ do
      let
        sup =
          Superset
            [aesonQQ|
          { "foo": { "bar": 1, "baz": "baz" }
          , "bat": { "bat": 1 }
          }
        |]

        sub =
          Subset
            [aesonQQ|
          { "foo": { "bar": "baz" }
          , "bat": "bat"
          }
        |]

      pruneJson sup sub
        `shouldBeJson` [aesonQQ|
        { "foo": { "bar": 1 }
        , "bat": { "bat": 1 }
        }
      |]

  describe "sortJsonArrays" $ do
    it "sorts arrays" $ do
      let
        unsorted = [aesonQQ|["number_facts", "number_basics"]|]
        sorted = [aesonQQ|["number_basics", "number_facts"]|]

      sortJsonArrays unsorted `shouldBeJson` sorted

    it "sorts arrays in object keys" $ do
      let
        unsorted = [aesonQQ|{ "x": ["number_facts", "number_basics"] }|]
        sorted = [aesonQQ|{ "x": ["number_basics", "number_facts"] }|]

      sortJsonArrays unsorted `shouldBeJson` sorted

    it "works on arrays of nested arrays" $ do
      let
        unsorted = [aesonQQ|[{ "x": ["number_facts", "number_basics"] }]|]
        sorted = [aesonQQ|[{ "x": ["number_basics", "number_facts"] }]|]

      sortJsonArrays unsorted `shouldBeJson` sorted
