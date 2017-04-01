{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphula.Persist (withPersistGraph) where

import Graphula (Graph, Actions(..))
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql

withPersistGraph :: (SqlBackendCanRead backend, PersistQueryWrite backend, PersistStoreWrite backend, PersistUniqueWrite backend, MonadIO m) => Graph (PersistRecord backend) Entity r -> ReaderT backend m r
withPersistGraph f = flip iterM f $ \case
  Insert n next -> do
    mKey <- insertUnique n
    case mKey of
      Nothing -> next Nothing
      Just key -> next =<< getEntity key
  LiftIO io next ->
    next =<< liftIO io

class (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where

instance (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where
