{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphula.Persist (withPersistGraph) where

import Graphula (Graph, Actions(..))
import Control.Monad.Trans.Free (iterTM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql

withPersistGraph
  :: (SqlBackendCanWrite backend, MonadIO m)
  => Graph (PersistRecord backend) Entity m r -> ReaderT backend m r
withPersistGraph f = flip iterTM f $ \case
  Insert n next -> do
    mKey <- insertUnique n
    case mKey of
      Nothing -> next Nothing
      Just key -> next =<< getEntity key

class (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where

instance (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where
