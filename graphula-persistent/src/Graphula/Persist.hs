{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Graphula.Persist (GraphulaPersistT, runGraphulaPersistT, onlyKey, keys, Keys, PersistRecord) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Graphula

newtype RunDB backend n m = RunDB (forall b. ReaderT backend n b -> m b)

newtype GraphulaPersistT backend n m a =
  GraphulaPersistT { runGraphulaPersistT' :: ReaderT (RunDB backend n m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RunDB backend n m))

instance MonadUnliftIO m => MonadUnliftIO (GraphulaPersistT backend n m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = GraphulaPersistT $ withUnliftIO $ \u ->
    return $ UnliftIO $ unliftIO u . runGraphulaPersistT'
  {-# INLINE withRunInIO #-}
  withRunInIO inner = GraphulaPersistT $ withRunInIO $ \run ->
    inner $ run . runGraphulaPersistT'

instance MonadTrans (GraphulaPersistT backend n) where
  lift = GraphulaPersistT . lift

class (Key a ~ ExternalKey a, PersistRecord backend a) => PersistNodeConstraint backend a
instance (Key a ~ ExternalKey a, PersistRecord backend a) => PersistNodeConstraint backend a

instance (MonadIO m, Applicative n, MonadIO n, SqlBackendCanWrite backend, BaseBackend backend ~ SqlBackend) => MonadGraphulaFrontend (GraphulaPersistT backend n m) where
  type NodeConstraint (GraphulaPersistT backend n m) = PersistNodeConstraint backend
  type Node (GraphulaPersistT backend n m) = Entity
  insert mKey0 n = do
    RunDB runDB <- ask
    lift . runDB $
      case mKey0 of
        -- No external key, do the easy thing
        Nothing -> do
          mKey1 <- insertUnique n
          case mKey1 of
            Nothing -> pure Nothing
            Just key1 -> getEntity key1
        Just key0 -> do
          mOther <- get key0
          case mOther of
            -- Key exists, try again
            Just {} -> pure Nothing
            Nothing -> do
              -- Check other uniques
              mConflicts <- checkUnique n
              case mConflicts of
                Nothing -> do
                  insertKey key0 n
                  pure $ Just $ Entity key0 n
                Just {} -> pure Nothing
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
