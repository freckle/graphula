{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal type class(es) for Graphula-related behaviors
module Graphula.Class
  ( MonadGraphula
  , MonadGraphulaFrontend(..)
  , MonadGraphulaBackend(..)
  , GraphulaSafeToInsert
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Kind (Constraint, Type)
import Database.Persist (Entity(..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import Test.QuickCheck.Random (QCGen)
#if MIN_VERSION_persistent(2,14,0)
import Database.Persist.Class.PersistEntity
#endif

-- | A class that provides backwards compatibility with @persistent-2.14@
--
-- If you are using that version or above, then this is a class alias for
-- 'SafeToInsert'. Otherwise, it is an identity alias.
--
type GraphulaSafeToInsert a =
#if MIN_VERSION_persistent(2,14,0)
    SafeToInsert a
#else
    () :: Constraint
#endif

type MonadGraphula m
  = (Monad m, MonadIO m, MonadGraphulaBackend m, MonadGraphulaFrontend m)

class MonadGraphulaFrontend m where
  insert
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m, GraphulaSafeToInsert a)
    => Maybe (Key a)
    -> a
    -> m (Maybe (Entity a))

  remove
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m)
    => Key a
    -> m ()

class MonadGraphulaBackend m where
  type Logging m :: Type -> Constraint
  askGen :: m (IORef QCGen)
  logNode :: Logging m a => a -> m ()
