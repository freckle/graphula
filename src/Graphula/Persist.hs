{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphula.Persist (persistGraph) where

import Graphula
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql

persistGraph
  :: (SqlBackendCanWrite backend, MonadIO m)
  => Frontend (PersistRecord backend) Entity (ReaderT backend m r) -> ReaderT backend m r
persistGraph = \case
  Insert n next -> do
    mKey <- insertUnique n
    case mKey of
      Nothing -> next Nothing
      Just key -> next =<< getEntity key

class (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where

instance (PersistEntity a, PersistEntityBackend a ~ SqlBackend, PersistStoreWrite backend, PersistUniqueWrite backend) => PersistRecord backend a where
