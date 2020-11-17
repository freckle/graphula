# Graphula Core

Graphula is a simple interface for generating persistent data and linking its dependencies. We use this interface to generate fixtures for automated testing. The interface is extensible and supports pluggable front-ends.


<!--
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Typeable
import GHC.Generics (Generic)
import Graphula
import Graphula.UUIDKey
import Test.Hspec
import Test.QuickCheck

instance (ToBackendKey SqlBackend a) => Arbitrary (Key a) where
  arbitrary = toSqlKey <$> arbitrary
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
  Id UUIDKey
  a Int
  b String
  deriving Show Eq Generic

E
  Id DId sqltype=uuid
  a String
  deriving Show Eq Generic

F
  a Bool
  UniqueFA a
  deriving Show Eq Generic
|]

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary D where
  arbitrary = D <$> arbitrary <*> arbitrary

instance Arbitrary E where
  arbitrary = E <$> arbitrary

instance Arbitrary F where
  arbitrary = F <$> arbitrary
```

## Dependencies

We declare dependencies via the `HasDependencies` typeclass and its associated type `Dependencies`.

By default a type does not have any dependencies. We only need to declare an empty instance.

```haskell
instance HasDependencies A
instance HasDependencies F
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

Graphula supports non-sequential keys with the `KeySource` associated type. To generate a key using
its `Arbitrary` instance, use `'SourceArbitrary`. Non-serial keys will need to also derive
an overlapping `Arbitrary` instance.

```haskell
instance HasDependencies D where
  type KeySource D = 'SourceArbitrary

deriving instance {-# OVERLAPPING #-} Arbitrary (Key D)
```

You can also elect to always specify an external key using `'SourceExternal`. This means that
this type cannot be constructed with `node`; use `nodeKeyed` instead.

```haskell
instance HasDependencies E where
  type KeySource E = 'SourceExternal
```

By default, `HasDependencies` instances use `type KeySource _ = 'SourceDefault`, which means
that graphula will expect the database to provide a key.

## Serialization

Graphula allows logging of graphs via `runGraphulaLogged`. Graphula dumps graphs
to a temp file on test failure.

```haskell
loggingSpec :: IO ()
loggingSpec = do
  let
    logFile = "test.graphula"
    -- We'd typically use `runGraphulaLogged` which utilizes a temp file.
    failingGraph = runGraphulaT Nothing runDB . runGraphulaLoggedWithFileT logFile $ do
      Entity _ a <- node @A () $ edit $ \n ->
        n {aA = "success"}
      liftIO $ aA a `shouldBe` "failed"

  failingGraph
    `shouldThrow` anyException

  n <- lines <$> readFile "test.graphula"
  n `shouldSatisfy` (not . null)
```

## Running It

```haskell
simpleSpec :: IO ()
simpleSpec =
  runGraphulaT Nothing runDB $ do
    -- Declare the graph at the term level
    Entity aId _ <- node @A () mempty
    liftIO $ putStrLn "A"
    Entity bId b <- node @B (only aId) mempty
    -- Type application is not necessary, but recommended for clarity.
    liftIO $ putStrLn "B"
    Entity _ c <- node @C (aId, bId) $ edit $ \n ->
      n { cC = "edited" }
    liftIO $ putStrLn "C"
    Entity dId _ <- node @D () mempty
    liftIO $ putStrLn "D"
    Entity eId _ <- nodeKeyed @E (EKey dId) () mempty
    liftIO $ putStrLn "E"

    -- Do something with your data
    liftIO $ do
      cC c `shouldBe` "edited"
      cA c `shouldBe` bA b
      unEKey eId `shouldBe` dId
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
    failingGraph =  runGraphulaT Nothing runDB . runGraphulaFailT $ do
      Entity _ _ <- node @A () mempty
      pure ()
  failingGraph
    `shouldThrow` (== (GenerationFailureMaxAttemptsToInsert (typeRep $ Proxy @A)))
```

Note that graphula can fail naturally if we define a graph that violates unique constraints
in the database:

```haskell
constraintFailureSpec :: IO ()
constraintFailureSpec = do
  let
    failingGraph =  runGraphulaT Nothing runDB $
      replicateM_ 3 $ node @F () mempty
  failingGraph
    `shouldThrow` (== (GenerationFailureMaxAttemptsToInsert (typeRep $ Proxy @F)))
```

or if we define a graph with an unsatisfiable predicates:

```haskell
ensureFailureSpec :: IO ()
ensureFailureSpec = do
  let
    failingGraph =  runGraphulaT Nothing runDB $ do
      Entity _ _ <- node @A () $ ensure $ \a -> a /= a
      pure ()
  failingGraph
    `shouldThrow` (== (GenerationFailureMaxAttemptsToConstrain (typeRep $ Proxy @A)))
```

<!--
```haskell
main :: IO ()
main = hspec $
  describe "graphula-core" . parallel $ do
    it "generates and links arbitrary graphs of data" simpleSpec
    it "allows logging graphs" loggingSpec
    it "attempts to retry node generation on insertion failure" insertionFailureSpec
    it "attempts to retry node generation on a database constraint violation" constraintFailureSpec
    it "attempts to retry node generation on unsatisfiable predicates" ensureFailureSpec

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "test.db" $ do
  runMigration migrateAll
  f
```
-->
