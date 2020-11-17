{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphula.UUIDKey
  ( UUIDKey
  )
where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist
import Database.Persist.Sql
import Test.QuickCheck (Arbitrary(..), Gen, getLarge)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece(..))

-- | Example non-serial key
newtype UUIDKey = UUIDKey { unUUIDKey :: UUID }
  deriving newtype (Eq, Show, Ord, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

instance Arbitrary UUIDKey where
  arbitrary = UUIDKey <$> uuid
    where uuid = UUID.fromWords <$> large <*> large <*> large <*> large

large :: (Integral a, Bounded a) => Gen a
large = getLarge <$> arbitrary

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
