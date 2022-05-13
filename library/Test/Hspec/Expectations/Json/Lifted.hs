module Test.Hspec.Expectations.Json.Lifted
  ( shouldBeJson
  , shouldBeUnorderedJson
  , shouldMatchJson
  , shouldMatchOrderedJson
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import GHC.Stack
import qualified Test.Hspec.Expectations.Json as E

shouldBeJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldBeJson x = liftIO . E.shouldBeJson x

shouldBeUnorderedJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldBeUnorderedJson x = liftIO . E.shouldBeUnorderedJson x

shouldMatchJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldMatchJson x = liftIO . E.shouldMatchJson x

shouldMatchOrderedJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldMatchOrderedJson x = liftIO . E.shouldMatchOrderedJson x
