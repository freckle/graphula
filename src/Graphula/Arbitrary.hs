-- | 'Arbitrary' operations that respect Graphula's seed
{-# OPTIONS_GHC -Wno-deprecations #-}
module Graphula.Arbitrary
  ( generate
  ) where

import Prelude

import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Data.IORef (readIORef, writeIORef)
import Graphula.Class (MonadGraphulaBackend, askGen)
import System.Random (split)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (unGen)

-- | Run a generator
--
-- This is akin to 'Test.QuickCheck.generate', but utilizing graphula's
-- generation. The size passed to the generator is always 30; if you want
-- another size then you should explicitly use 'Test.QuickCheck.resize'.
generate :: (MonadIO m, MonadGraphulaBackend m) => Gen a -> m a
generate gen = do
  genRef <- askGen
  g <- liftIO $ readIORef genRef
  let
    (g1, g2) = split g
    x = unGen gen g1 30
  liftIO $ writeIORef genRef g2
  pure x
