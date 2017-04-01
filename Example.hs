{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Graphula
import Data.Functor.Identity (Identity(..))
import Control.Monad.Free (iterM)

main :: IO ()
main =
  -- wrap computation to allow shrinking, saving failed states, replay, etc with a free monad
  withGraphIO $ do
    -- Declare the graph at the term level
    a <- node @A
    b <- nodeWith @B a -- This can actually be inferred
    c <- nodeEditWith @C (a, b) $ \n ->
      n { cc = "spanish" }

    -- do shit with it
    liftIO $ print (a, b, c)


withGraphIO :: Graph Identity r -> IO r
withGraphIO f = flip iterM f $ \case
  Insert n next ->
    next $ Just $ Identity n
  Validate _ next ->
    next True
  LiftIO io next ->
    next =<< io


data A = A { aa :: String, ab :: Int }
  deriving (Show)

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance HasDependencies A

data B = B { ba :: A, bb :: String }
  deriving (Show)

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance HasDependencies B where
  type Dependencies B = Identity A
  dependsOn b a = b {ba = runIdentity a}


data C = C { ca :: A, cb :: B , cc :: String}
  deriving (Show)

instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance HasDependencies C where
  type Dependencies C = (Identity A, Identity B)
  dependsOn c (a, b) = C (runIdentity a) (runIdentity b) (cc c)
