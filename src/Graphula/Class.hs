{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphula.Class
  ( MonadGraphula
  , MonadGraphulaFrontend(..)
  , MonadGraphulaBackend(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Kind (Constraint, Type)
import Database.Persist (Entity(..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import Test.QuickCheck.Random (QCGen)

type MonadGraphula m
  = (Monad m, MonadIO m, MonadGraphulaBackend m, MonadGraphulaFrontend m)

class MonadGraphulaFrontend m where
  insert
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m)
    => Maybe (Key a)
    -> a
    -> m (Maybe (Entity a))

  remove
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m)
    => Key a
    -> m ()

class MonadGraphulaBackend m where
  type Logging m :: Type -> Constraint
  -- ^ A constraint provided to log details of the graph to some form of
  --   persistence. This is used by 'runGraphulaLogged' to store graph nodes as
  --   'Show'n 'Text' values
  askGen :: m (IORef QCGen)
  logNode :: Logging m a => a -> m ()
