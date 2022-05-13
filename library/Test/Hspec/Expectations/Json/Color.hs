module Test.Hspec.Expectations.Json.Color
  ( Color(..)
  , getColorize
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)

data Color = Reset | Red | Green

getColorize :: MonadIO m => m (Color -> String -> String)
getColorize = do
  -- The stdout handle will not appear as a terminal on GitHub Actions, but it
  -- does support color escapes.
  shouldColorize <-
    liftIO $ (||) <$> isGitHubActions <*> hIsTerminalDevice stdout

  pure $ if shouldColorize
    then \c x -> escape Reset <> escape c <> x <> escape Reset
    else \_ x -> x
  where isGitHubActions = (== Just "true") <$> lookupEnv "GITHUB_ACTIONS"

escape :: Color -> String
escape = \case
  Reset -> "\ESC[0m"
  Red -> "\ESC[0;31m"
  Green -> "\ESC[0;32m"
