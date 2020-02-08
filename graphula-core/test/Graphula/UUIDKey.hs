{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphula.UUIDKey
  ( UUIDKey
  )
where

import Prelude

import Data.UUID (UUID)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import Database.Persist
import Database.Persist.Sql
import Test.QuickCheck (Arbitrary(..), getLarge)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)
import Web.PathPieces (PathPiece(..))

-- | Example non-serial key
newtype UUIDKey = UUIDKey { unUUIDKey :: UUID }
  deriving newtype (Eq, Show, Ord, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

instance Arbitrary UUIDKey where
  arbitrary = UUIDKey <$> uuid
   where
    uuid = UUID.fromWords <$> word <*> word <*> word <*> word
    word = getLarge <$> arbitrary

instance PathPiece UUIDKey where
  toPathPiece = Text.pack . UUID.toString . unUUIDKey
  fromPathPiece = fmap UUIDKey . UUID.fromString . Text.unpack

instance PersistField UUIDKey where
  toPersistValue = PersistText . Text.pack . UUID.toString . unUUIDKey
  fromPersistValue = \case
    PersistText t -> case UUID.fromString $ Text.unpack t of
      Just x -> Right $ UUIDKey x
      Nothing -> Left "Invalid UUID"
    _ -> Left "Not PersistText"

instance PersistFieldSql UUIDKey where
  sqlType _ = SqlOther "uuid"
