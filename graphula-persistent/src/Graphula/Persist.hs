{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphula.Persist (GraphulaPersistT, runGraphulaPersistT, onlyKey, keys, Keys, PersistRecord) where

import Graphula
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Database.Persist
import Database.Persist.Sql
import GHC.TypeLits (TypeError, ErrorMessage(..))

newtype RunDB backend n m = RunDB (forall b. ReaderT backend n b -> m b)

newtype GraphulaPersistT backend n m a =
  GraphulaPersistT { runGraphulaPersistT' :: ReaderT (RunDB backend n m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader (RunDB backend n m))

instance MonadTrans (GraphulaPersistT backend n) where
  lift = GraphulaPersistT . lift

instance (MonadIO m, Applicative n, MonadIO n, SqlBackendCanWrite backend) => MonadGraphulaFrontend (GraphulaPersistT backend n m) where
  type NodeConstraint (GraphulaPersistT backend n m) = PersistRecord backend
  type Node (GraphulaPersistT backend n m) = Entity
  insert n = do
    RunDB runDB <- ask
    lift . runDB $ do
      mKey <- insertUnique n
      case mKey of
        Nothing -> pure Nothing
        Just key' -> getEntity key'
  remove ent = do
    RunDB runDB <- ask
    lift . runDB . delete $ entityKey ent

runGraphulaPersistT
  :: (SqlBackendCanWrite backend, MonadIO m)
  => (forall b. ReaderT backend n b -> m b) -> GraphulaPersistT backend n m a -> m a
runGraphulaPersistT runDB action =
  runGraphulaPersistT' action `runReaderT` RunDB runDB

class (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where

instance (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where


class EntityKeys a where
  type Keys a
  keys :: a -> Keys a

instance
  ( TypeError
    ( 'Text "Cannot use naked ‘" ':<>: 'ShowType (Entity a) ':<>:
      'Text "’ as argument to ‘keys’." ':$$:
      'Text "Did you mean ‘Only (" ':<>:
      'ShowType (Entity a) ':<>: 'Text ")’?"
    )
  ) => EntityKeys (Entity a) where
  type Keys (Entity a) = Key a
  keys = entityKey

onlyKey :: Entity a -> Only (Key a)
onlyKey = keys . only

instance EntityKeys (Only (Entity a)) where
  type Keys (Only (Entity a)) = Only (Key a)
  keys (Only a) = Only (entityKey a)

instance EntityKeys (Entity a, Entity b) where
  type Keys (Entity a, Entity b) = (Key a, Key b)
  keys (a, b) = (entityKey a, entityKey b)

instance EntityKeys (Entity a, Entity b, Entity c) where
  type Keys (Entity a, Entity b, Entity c) = (Key a, Key b, Key c)
  keys (a, b, c) = (entityKey a, entityKey b, entityKey c)

instance EntityKeys (Entity a, Entity b, Entity c, Entity d) where
  type Keys (Entity a, Entity b, Entity c, Entity d) = (Key a, Key b, Key c, Key d)
  keys (a, b, c, d) = (entityKey a, entityKey b, entityKey c, entityKey d)
