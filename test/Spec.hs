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

import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.IO.Class
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


withGraph test = do
  liftIO . runSqlite ":test:" $ do
    runMigration migrateAll
    runGraphula persistGraph test
  1 `shouldBe` 1

main :: IO ()
main = hspec $ do
  describe "trivial test" $ do
    it "should persist things correctly" $ do
      withGraph $ do
        a <- node @AT
        b <- nodeWith @BT (entityKey a)
        c <- nodeEditWith @CT (entityKey a, entityKey b) $ \n ->
          n { cTC = "spanish" }

        Just persistedC <- lift . getEntity $ entityKey c
        liftIO $ persistedC `shouldBe` c
