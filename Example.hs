{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Aeson
import Data.Functor.Identity (Identity(..))
import Control.Monad.IO.Class
import Graphula
import GHC.Generics (Generic)
import Test.QuickCheck

main :: IO ()
main =
  -- wrap computation to allow shrinking, saving failed states, replay, etc with a free monad
  runGraphula graphIO $ do
    -- Declare the graph at the term level
    a <- node @A
    b <- nodeWith @B a -- This can actually be inferred
    c <- nodeEditWith @C (a, b) $ \n ->
      n { cc = "spanish" }

    -- do shit with it
    liftIO $ print (a, b, c)


graphIO :: Frontend NoConstraint Identity (IO r) -> IO r
graphIO = \case
  Insert n next ->
    next $ Just $ Identity n

data A = A { aa :: String, ab :: Int }
  deriving (Show, Generic)

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance HasDependencies A

instance ToJSON A
instance FromJSON A

data B = B { ba :: A, bb :: String }
  deriving (Show, Generic)

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance HasDependencies B where
  type Dependencies B = Identity A
  dependsOn b a = b {ba = runIdentity a}

instance ToJSON B
instance FromJSON B


data C = C { ca :: A, cb :: B , cc :: String}
  deriving (Show, Generic)

instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance HasDependencies C where
  type Dependencies C = (Identity A, Identity B)
  dependsOn c (a, b) = C (runIdentity a) (runIdentity b) (cc c)

instance ToJSON C
instance FromJSON C
