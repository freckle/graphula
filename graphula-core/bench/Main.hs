{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Criterion.Main
import Graphula
import qualified Graphula.Free as Free
import Test.QuickCheck.Arbitrary
import Control.Monad.Trans
import Control.Monad.Catch
import Data.Functor.Identity
import Data.Aeson
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
  , bgroup "logged"
    [ bench "1"  . nfIO $ replicateNodeLogged 1
    , bench "100"  . nfIO $ replicateNodeLogged 100
    , bench "1000"  . nfIO $ replicateNodeLogged 1000
    ]
  , bgroup "idempotent"
    [ bench "1"  . nfIO $ replicateNodeIdempotent 1
    , bench "100"  . nfIO $ replicateNodeIdempotent 100
    , bench "1000"  . nfIO $ replicateNodeIdempotent 1000
    ]
  ]

data A
  = A
  { aa :: String
  , ab :: Int
  } deriving (Generic)

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance ToJSON A

instance HasDependencies A

graphIdentity :: Free.Frontend NoConstraint Identity (IO r) -> IO r
graphIdentity f = case f of
  Free.Insert n next ->
    next $ Just $ Identity n
  Free.Remove _ next ->
    next

replicateNodeInitial :: Int -> IO ()
replicateNodeInitial i = void . Free.runGraphula graphIdentity . replicateM i $ node @A

newtype GraphulaIdentity a = GraphulaIdentity { runGraphulaIdentity :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadGraphulaFrontend GraphulaIdentity where
  type NodeConstraint GraphulaIdentity = NoConstraint
  type Node GraphulaIdentity = Identity
  insert = pure . Just . Identity
  remove = const (pure ())

replicateNodeFinal :: Int -> IO ()
replicateNodeFinal i = void . runGraphulaIdentity . runGraphulaT . replicateM i $ node @A

replicateNodeLogged :: Int -> IO ()
replicateNodeLogged i = void . runGraphulaIdentity . runGraphulaLoggedT . replicateM i $ node @A

replicateNodeIdempotent :: Int -> IO ()
replicateNodeIdempotent i = void . runGraphulaIdentity . runGraphulaIdempotentT . runGraphulaT . replicateM i $ node @A
