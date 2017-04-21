```haskell
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Aeson
import Database.Persist
import Database.Persist.Arbitrary()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Graphula
import Graphula.Persist
import Test.Hspec
import Test.QuickCheck
```

## Declare Models
```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    AT
      a String
      b Int
      deriving Show Eq Generic

    BT
      a ATId
      b String
      deriving Show Eq Generic

    CT
      a ATId
      b BTId
      c String
      deriving Show Eq Generic

    DT
      c CTId
      flag Bool
      UniqueFlag flag
      deriving Show Eq Generic
  |]
```

## Generate Relational Fixtures
```haskell
main :: IO ()
main =
  hspec $
  beforeAll (runTestDB $ migrateTestDB *> truncateTestDB) $
  describe "graphula-persistent" $ do

    let makeSimpleGraph = do
          a <- node
          b <- nodeWith . keys $ only a
          c <- nodeEditWith (keys (a, b)) $ \n ->
            n { cTC = "spanish" }
          pure (a, b, c)

    it "should ensure graph values should match persisted values" $ withGraph $ do
      (a, b, c) <- makeSimpleGraph
      liftIO $ do
        Just persistedA <- runTestDB . getEntity $ entityKey a
        persistedA `shouldBe` a

        Just persistedB <- runTestDB . getEntity $ entityKey b
        persistedB `shouldBe` b

        Just persistedC <- runTestDB . getEntity $ entityKey c
        persistedC `shouldBe` c

    it "should respect unique constraints" $ withGraph $ do
      (_, _, c) <- makeSimpleGraph
      d1 <- nodeWith . keys $ only c
      d2 <- nodeWith . keys $ only c
      liftIO $ do
        (persistedD1, persistedD2) <- runTestDB $ do
          Just persistedD1 <- get $ entityKey d1
          Just persistedD2 <- get $ entityKey d2
          pure (persistedD1, persistedD2)

        dTFlag persistedD1 `shouldNotBe` dTFlag persistedD2
        persistedD1 `shouldBe` entityVal d1
        persistedD2 `shouldBe` entityVal d2
```


```haskell
instance Arbitrary AT where
  arbitrary = AT <$> arbitrary <*> arbitrary

instance HasDependencies AT

instance ToJSON AT
instance FromJSON AT


instance Arbitrary BT where
  arbitrary = BT <$> arbitrary <*> arbitrary

instance HasDependencies BT where
  type Dependencies BT = Only (Key AT)

instance ToJSON BT
instance FromJSON BT


instance Arbitrary CT where
  arbitrary = CT <$> arbitrary <*> arbitrary <*> arbitrary

instance HasDependencies CT where
  type Dependencies CT = (Key AT, Key BT)

instance ToJSON CT
instance FromJSON CT

instance Arbitrary DT where
  arbitrary = DT <$> arbitrary <*> arbitrary

instance HasDependencies DT where
  type Dependencies DT = Only (Key CT)

instance ToJSON DT
instance FromJSON DT
```

```haskell
migrateTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migrateTestDB = runMigration migrateAll

truncateTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
truncateTestDB = do
  deleteWhere ([] :: [Filter DT])
  deleteWhere ([] :: [Filter CT])
  deleteWhere ([] :: [Filter BT])
  deleteWhere ([] :: [Filter AT])

runTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runTestDB = runSqlite ":test:"

withGraph :: Graph Arbitrary ToJSON (PersistRecord SqlBackend) Entity IO b -> IO b
withGraph = runGraphulaLogged (persistGraph runTestDB)
```
