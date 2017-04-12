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
  type Dependencies BT = Key AT
  dependsOn b a = b {bTA = a}

instance ToJSON BT
instance FromJSON BT


instance Arbitrary CT where
  arbitrary = CT <$> arbitrary <*> arbitrary <*> arbitrary

instance HasDependencies CT where
  type Dependencies CT = (Key AT, Key BT)
  dependsOn c (a, b) = c { cTA = a, cTB = b }

instance ToJSON CT
instance FromJSON CT


main :: IO ()
main =
  hspec $
  beforeAll (runTestDB $ runMigration migrateAll) $
  describe "trivial test" $ do
    it "should persist things correctly" $ withGraph $ do
      a <- node
      b <- nodeWith $ keys a
      c <- nodeEditWith (keys (a, b)) $ \n ->
        n { cTC = "spanish" }
      Just persistedC <- liftIO . runTestDB . getEntity $ entityKey c
      liftIO $ persistedC `shouldBe` c

runTestDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runTestDB = runSqlite ":test:"

withGraph :: Graph (PersistRecord SqlBackend) Entity IO b -> IO b
withGraph = runGraphula (persistGraph runTestDB)
