{-# LANGUAGE QuasiQuotes #-}

module Test.Hspec.Expectations.JsonSpec
  ( spec
  )
where

import Prelude

import Data.Aeson.QQ
import Test.Hspec
import Test.Hspec.Expectations.Json

spec :: Spec
spec = do
  describe "shouldMatchJson" $ do
    it "passes regardless of array order" $ do
      let
        a = [aesonQQ|[{ "foo": 1 }, { "foo": 0 }]|]
        b = [aesonQQ|[{ "foo": 0 }, { "foo": 1 }]|]

      a `shouldMatchJson` b

    it "passes regardless of array order at depth" $ do
      let
        a = [aesonQQ|{ "a": [{ "foo": 1 }, { "foo": 0 }] }|]
        b = [aesonQQ|{ "a": [{ "foo": 0 }, { "foo": 1 }] }|]

      a `shouldMatchJson` b

    it "passes regardless of extra keys" $ do
      let
        a = [aesonQQ|{ "foo": "bar", "baz": "bat" }|]
        b = [aesonQQ|{ "foo": "bar" }|]

      a `shouldMatchJson` b

    it "passes regardless of extra keys at depth" $ do
      let
        a = [aesonQQ|{ "a": { "foo": "bar", "baz": "bat" } }|]
        b = [aesonQQ|{ "a": { "foo": "bar" } }|]

      a `shouldMatchJson` b

    it "matches pruned and unsorted array elements" $ do
      let
        sup1 = [aesonQQ|{ "studentId": 1, "x": true }|]
        sub1 = [aesonQQ|{ "studentId": 1 }|]
        sup2 = [aesonQQ|{ "studentId": 2, "x": true }|]
        sub2 = [aesonQQ|{ "studentId": 2 }|]
        sup3 = [aesonQQ|{ "studentId": 3, "x": true }|]
        sub3 = [aesonQQ|{ "studentId": 3 }|]
        sup4 = [aesonQQ|{ "studentId": 4, "x": true }|]
        sub4 = [aesonQQ|{ "studentId": 4 }|]

        a = [aesonQQ|
          [ { "stats": [#{sup3}] }
          , { "stats": [#{sup3}, #{sup1}] }
          , { "stats": [#{sup4}, #{sup2}] }
          , { "stats": [#{sup2}] }
          , { "stats": [#{sup4}] }
          , { "stats": [#{sup4}] }
          ]
        |]

        b = [aesonQQ|
          [ { "stats": [#{sub1}, #{sub3}] }
          , { "stats": [#{sub3}] }
          , { "stats": [#{sub2}, #{sub4}] }
          , { "stats": [#{sub2}] }
          , { "stats": [#{sub4}] }
          , { "stats": [#{sub4}] }
          ]
        |]

      a `shouldMatchJson` b

    it "handles cases where sorting differs after pruning" $ do
      let
        a = [aesonQQ|
          [ { "shortName": "B"
            , "subSkills":
              [ { "shortName": "1"
                , "uspId": "68fa57ddbc1e9aa332f7c88884e5c40e"
                }
              ]
            }
          , { "shortName": "A"
            , "subSkills":
                 [ { "shortName": "a"
                   , "uspId": "71839723561ba1a49cf2a789dbe50302"
                   }
                 , { "shortName": "b"
                   , "uspId": "4096d2cfebcab73438971ae2304544ee"
                   }
                 ]
            }
          ]
        |]
        b = [aesonQQ|
          [ { "subSkills":
              [ { "uspId": "71839723561ba1a49cf2a789dbe50302" }
              , { "uspId": "4096d2cfebcab73438971ae2304544ee" }
              ]
            }
          , { "subSkills":
              [ { "uspId": "68fa57ddbc1e9aa332f7c88884e5c40e" }
              ]
            }
          ]
        |]

      a `shouldMatchJson` b
