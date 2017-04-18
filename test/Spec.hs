{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Graphula
import Graphula.Persist
import Test.Hspec
import Test.QuickCheck

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

instance (ToBackendKey SqlBackend a) => Arbitrary (Key a) where
  arbitrary = toSqlKey <$> arbitrary

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

migrateTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migrateTestDB = runMigration migrateAll

truncateTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
truncateTestDB = do
  deleteWhere ([] :: [Filter DT])
  deleteWhere ([] :: [Filter CT])
  deleteWhere ([] :: [Filter BT])
  deleteWhere ([] :: [Filter AT])

main :: IO ()
main =
  hspec $
  beforeAll (runTestDB $ migrateTestDB *> truncateTestDB) $
  describe "trivial test" $ do
    it "should persist things correctly" $ withGraph $ do
      a <- node
      b <- nodeWith $ keys $ only a
      c <- nodeEditWith (keys (a, b)) $ \n ->
        n { cTC = "spanish" }
      Entity d1Id _ <- nodeWith $ keys $ only c
      Entity d2Id _ <- nodeWith $ keys $ only c
      Just persistedC <- liftIO . runTestDB . getEntity $ entityKey c
      liftIO $ persistedC `shouldBe` c
      (d1, d2) <- liftIO . runTestDB $ do
        Just d1 <- get d1Id
        Just d2 <- get d2Id
        pure (d1, d2)
      liftIO $ dTFlag d1 `shouldNotBe` dTFlag d2

runTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runTestDB = runSqlite ":test:"

withGraph :: Graph (PersistRecord SqlBackend) Entity IO b -> IO b
withGraph = runGraphula (persistGraph runTestDB)
