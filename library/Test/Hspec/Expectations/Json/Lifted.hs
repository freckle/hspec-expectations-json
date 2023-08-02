module Test.Hspec.Expectations.Json.Lifted
  ( shouldMatchJson
  , shouldBeJsonNormalized
  , E.Normalizer
  , E.treatNullsAsMissing
  , E.ignoreArrayOrdering
  , E.subsetActualToExpected
  , E.expandHeterogenousArrays
  -- Legacy API
  , shouldBeJson
  , shouldBeUnorderedJson
  , shouldMatchOrderedJson
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import GHC.Stack
import qualified Test.Hspec.Expectations.Json as E

shouldBeJsonNormalized
  :: (HasCallStack, MonadIO m) => E.Normalizer -> Value -> Value -> m ()
shouldBeJsonNormalized n x = liftIO . E.shouldBeJsonNormalized n x

shouldBeJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldBeJson x = liftIO . E.shouldBeJson x

shouldBeUnorderedJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldBeUnorderedJson x = liftIO . E.shouldBeUnorderedJson x

shouldMatchJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldMatchJson x = liftIO . E.shouldMatchJson x

shouldMatchOrderedJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldMatchOrderedJson x = liftIO . E.shouldMatchOrderedJson x
