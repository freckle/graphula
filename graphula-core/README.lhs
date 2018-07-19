# Graphula Core

Graphula is a simple interface for generating data and linking its dependencies. We use this interface to generate fixtures for automated testing. The interface is extensible and supports pluggable front-ends.


<!--
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Aeson
import Data.Typeable
import Data.Functor.Identity (Identity(..))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Graphula
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $
  describe "graphula-core" . parallel $ do
    it "generates and links arbitrary graphs of data" simpleSpec
    it "allows logging and replaying graphs" loggingAndReplaySpec
    it "attempts to retry node generation on insertion failure" insertionFailureSpec
```
-->

```haskell
simpleSpec :: IO ()
simpleSpec =
  runGraphulaIdentity . runGraphulaT $ do
    -- Declare the graph at the term level
    Identity a <- node @A
    Identity b <- nodeWith @B (only a)
    -- Type application is not necessary, but recommended for clarity.
    Identity c <- nodeEditWith @C (a, b) $ \n ->
      n { cc = "spanish" }

    -- Do something with your data
    liftIO $ do
      cc c `shouldBe` "spanish"
      ca c `shouldBe` ba b
```

## Arbitrary Data

Graphula utilizes `QuickCheck` to generate random data. We need to declare `Arbitrary` instances for our types.

```haskell
data A
  = A
  { aa :: String
  , ab :: Int
  } deriving (Show, Eq, Generic)

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary


data B
  = B
  { ba :: A
  , bb :: String
  } deriving (Show, Eq, Generic)

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary


data C
  = C
  { ca :: A
  , cb :: B
  , cc :: String
  } deriving (Show, Eq, Generic)

instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary
```

## Dependencies

We declare dependencies via the `HasDependencies` typeclass and its associated type `Dependencies`.

By default a type does not have any dependencies. We only need to declare an empty instance.

```haskell
instance HasDependencies A
```

For single dependencies we use the `Only` type.

```haskell
instance HasDependencies B where
  type Dependencies B = Only A
```

Groups of dependencies use tuples. Declare these dependencies in the order they appear in the type. `HasDependencies` leverages generic programming to inject dependencies for you.

```haskell
instance HasDependencies C where
  type Dependencies C = (A, B)
```

## Replay And Serialization

Graphula allows logging of graphs via `runGraphulaLogged`. We use `JSON` as a human readable serialization format. Graphula dumps graphs to a temp file on test failure. You can inspect or `runGraphulaReplay` a failed graph for red/green refactor.

```haskell
instance ToJSON A
instance FromJSON A

instance ToJSON B
instance FromJSON B

instance ToJSON C
instance FromJSON C

loggingAndReplaySpec :: IO ()
loggingAndReplaySpec = do
  let
    logFile = "test.graphula"
    -- We'd typically use `runGraphulaLogged` which utilizes a temp file.
    failingGraph = runGraphulaIdentity . runGraphulaLoggedWithFileT logFile $ do
      Identity a <- nodeEdit @A $ \n ->
        n {aa = "success"}
      liftIO $ aa a `shouldBe` "failed"
    replayGraph = runGraphulaIdentity . runGraphulaReplayT logFile $ do
      Identity a <- node @A
      liftIO $ aa a `shouldBe` "success"

  failingGraph
    `shouldThrow` anyException
  replayGraph
```

## Running It

`runGraphula` requires you to provide a front-end. This carries the instructions for evaluating a graph. Our simple `Frontend` is not constraining types and it is wrapping insert results in `Identity`. [`Graphula.Persist`](https://github.com/frontrowed/graphula/tree/master/graphula-persistent) is an example of a more complex frontend utilizing `Database.Persist`.

```haskell
newtype GraphulaIdentity a = GraphulaIdentity { runGraphulaIdentity :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadGraphulaFrontend GraphulaIdentity where
  type NodeConstraint GraphulaIdentity = NoConstraint
  type Node GraphulaIdentity = Identity
  insert = pure . Just . Identity
  remove = const (pure ())
```

We can create other front-ends. For example, a front-end that always fails to insert.

```haskell
newtype GraphulaFail a = GraphulaFail { runGraphulaFail :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadGraphulaFrontend GraphulaFail where
  type NodeConstraint GraphulaFail = NoConstraint
  type Node GraphulaFail = Identity
  insert _ = pure $ Nothing
  remove = const (pure ())

insertionFailureSpec :: IO ()
insertionFailureSpec = do
  let
    failingGraph =  runGraphulaFail . runGraphulaT $ do
      Identity _ <- node @A
      pure ()
  failingGraph
    `shouldThrow` (== (GenerationFailureMaxAttempts (typeRep $ Proxy @A)))
```
