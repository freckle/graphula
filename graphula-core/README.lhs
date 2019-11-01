# Graphula Core

Graphula is a simple interface for generating persistent data and linking its dependencies. We use this interface to generate fixtures for automated testing. The interface is extensible and supports pluggable front-ends.


<!--
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Aeson
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Graphula
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Hspec
import Database.Persist (Entity(..))
import Database.Persist.Arbitrary ()
import Database.Persist.Sqlite
import Database.Persist.TH
```
-->

## Arbitrary Data

Graphula utilizes `QuickCheck` to generate random data. We need to declare `Arbitrary` instances for our types.

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
A
  a String
  b Int
  deriving Show Eq Generic

B
  a AId
  b String
  deriving Show Eq Generic

C
  a AId
  b BId
  c String
  deriving Show Eq Generic
D
  Id String
  c Bool
  deriving Show Eq Generic
E
  a Int
  b Int
  deriving Show Eq Generic
  Primary a b
|]

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary D where
  arbitrary = D <$> arbitrary

instance Arbitrary E where
  arbitrary = E <$> (smallPositive <$> arbitrary) <*> (smallPositive <$> arbitrary)
   where
    smallPositive = getSmall . getPositive
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
  type Dependencies B = Only AId
```

Groups of dependencies use tuples. Declare these dependencies in the order they appear in the type. `HasDependencies` leverages generic programming to inject dependencies for you.

```haskell
instance HasDependencies C where
  type Dependencies C = (AId, BId)
```

## Non Sequential Keys

Graphula supports non-sequential keys by implementing the `genEntityKey` method typeclass. This allows us to provide optional key generation.

```haskell
instance HasDependencies D where
  genEntityKey = do
    key <- replicateM 6 $ elements ['a'..'z']
    pure $ Just $ DKey key
```

## Composite keys

Graphula supports composite keys which `persistent` stores inside the record itself. Set `KeyType a = 'CompositeKey` to mark such occurences. This defaults to `'SimpleKey`.

```haskell
instance HasDependencies E where
  type KeyType E = 'CompositeKey
  genEntityKey = do
    x <- elements [1..10]
    y <- elements [1..10]
    pure $ Just $ EKey x y
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

instance ToJSON D
instance FromJSON D

loggingAndReplaySpec :: IO ()
loggingAndReplaySpec = do
  let
    logFile = "test.graphula"
    -- We'd typically use `runGraphulaLogged` which utilizes a temp file.
    failingGraph = runGraphulaT runDB . runGraphulaLoggedWithFileT logFile $ do
      Entity _ a <- nodeEdit @A $ \n ->
        n {aA = "success"}
      liftIO $ aA a `shouldBe` "failed"
    replayGraph = runGraphulaT runDB . runGraphulaReplayT logFile $ do
      Entity _ a <- node @A
      liftIO $ aA a `shouldBe` "success"

  failingGraph
    `shouldThrow` anyException
  replayGraph
```

## Running It

```haskell
simpleSpec :: IO ()
simpleSpec =
  runGraphulaT runDB $ do
    -- Declare the graph at the term level
    Entity aId _ <- node @A
    Entity bId b <- nodeWith @B (only aId)
    -- Type application is not necessary, but recommended for clarity.
    Entity _ c <- nodeEditWith @C (aId, bId) $ \n ->
      n { cC = "spanish" }
    Entity {} <- node @D
    Entity eKey e <- node @E

    -- Do something with your data
    liftIO $ do
      cC c `shouldBe` "spanish"
      cA c `shouldBe` bA b
      eKey `shouldBe` EKey (eA e) (eB e)
```

`runGraphulaT` carries frontend instructions. If we'd like to override them we need to declare our own frontend.

For example, a front-end that always fails to insert.

```haskell
newtype GraphulaFailT m a = GraphulaFailT { runGraphulaFailT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadGraphulaBackend)

instance MonadGraphulaFrontend (GraphulaFailT m) where
  insert _ _ = pure Nothing
  remove = const (pure ())

insertionFailureSpec :: IO ()
insertionFailureSpec = do
  let
    failingGraph =  runGraphulaT runDB . runGraphulaFailT $ do
      Entity _ _ <- node @A
      pure ()
  failingGraph
    `shouldThrow` (== (GenerationFailureMaxAttempts (typeRep $ Proxy @A)))
```

<!--
```haskell
main :: IO ()
main = hspec $
  describe "graphula-core" . parallel $ do
    it "generates and links arbitrary graphs of data" simpleSpec
    it "allows logging and replaying graphs" loggingAndReplaySpec
    it "attempts to retry node generation on insertion failure" insertionFailureSpec

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "test.db" $ do
  runMigration migrateAll
  f
```
-->
