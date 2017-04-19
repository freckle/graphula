{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphula.Persist (persistGraph, keys, PersistRecord) where

import Graphula
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql
import GHC.TypeLits (TypeError, ErrorMessage(..))

persistGraph
  :: (SqlBackendCanWrite backend, MonadIO m, MonadIO n)
  => (forall b. ReaderT backend n b -> m b)
  -> Frontend (PersistRecord backend) Entity (m a) -> m a
persistGraph runDB = \case
  Insert n next -> do
    x <- runDB $ do
      mKey <- insertUnique n
      case mKey of
        Nothing -> pure Nothing
        Just key -> getEntity key
    next x

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
