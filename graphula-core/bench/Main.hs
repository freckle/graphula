{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Criterion.Main
import Graphula
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (generate)
import Control.Monad.Trans
import Control.Monad.Catch
import Data.Functor.Identity
import Control.Monad (void, replicateM)
import GHC.Generics

main :: IO ()
main = defaultMain
  [ bgroup "initial algebra"
    [ bench "1"  . nfIO $ replicateNodeInitial 1
    , bench "100"  . nfIO $ replicateNodeInitial 100
    , bench "1000"  . nfIO $ replicateNodeInitial 1000
    ]
  , bgroup "final algebra"
    [ bench "1"  . nfIO $ replicateNodeFinal 1
    , bench "100"  . nfIO $ replicateNodeFinal 100
    , bench "1000"  . nfIO $ replicateNodeFinal 1000
    ]
  ]

data A
  = A
  { aa :: String
  , ab :: Int
  } deriving (Generic)

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance HasDependencies A

graphIdentity :: Frontend NoConstraint Identity (IO r) -> IO r
graphIdentity f = case f of
  Insert n next ->
    next $ Just $ Identity n
  Remove _ next ->
    next

replicateNodeInitial :: Int -> IO ()
replicateNodeInitial i = void . runGraphula graphIdentity . replicateM i $ node @A

newtype TagglessGraphula a = TagglessGraphula { runTagglessGraphula :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadGraphulaFrontend TagglessGraphula where
  type NodeConstraint TagglessGraphula = NoConstraint
  type Entity TagglessGraphula = Identity
  insert = pure . Just . Identity
  removeM = const (pure ())

instance MonadGraphulaBackend TagglessGraphula where
  type Logging TagglessGraphula = NoConstraint
  type Generate TagglessGraphula = Arbitrary
  generateNode = liftIO $ generate arbitrary
  logNode _ = pure ()

replicateNodeFinal :: Int -> IO ()
replicateNodeFinal i = void . runTagglessGraphula . replicateM i $ node @A
