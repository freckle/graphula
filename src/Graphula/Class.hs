{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal type class(es) for Graphula-related behaviors
module Graphula.Class
  ( MonadGraphula
  , MonadGraphulaFrontend (..)
  , MonadGraphulaBackend (..)
  , GraphulaSafeToInsert
  ) where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Kind (Constraint, Type)
import Database.Persist (Entity (..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import Test.QuickCheck.Random (QCGen)
#if MIN_VERSION_persistent(2,14,0)
import Database.Persist.Class.PersistEntity (SafeToInsert)
#endif

{- FOURMOLU_DISABLE -}

-- | A class that provides backwards compatibility with @persistent-2.14@
--
-- If you are using that version or above, then this is a class alias for
-- 'SafeToInsert'. Otherwise, it is an identity alias.
--
class
#if MIN_VERSION_persistent(2,14,0)
    (SafeToInsert a) =>
#else
    () =>
#endif
    GraphulaSafeToInsert a

instance
#if MIN_VERSION_persistent(2,14,0)
    (SafeToInsert a) =>
#else
    () =>
#endif
    GraphulaSafeToInsert a

{- FOURMOLU_ENABLE -}

type MonadGraphula m =
  (Monad m, MonadIO m, MonadGraphulaBackend m, MonadGraphulaFrontend m)

class MonadGraphulaFrontend m where
  insertVerbose
      :: ( PersistEntityBackend a ~ SqlBackend
       , PersistEntity a
       , Monad m
       , GraphulaSafeToInsert a
       )
    => Maybe (Key a)
    -> a
    -> m (Either (Maybe SomeException) (Entity a))
  insertVerbose mk a = insert mk a >>= \case
    Just ea -> pure (Right ea)
    Nothing -> pure (Left Nothing)

  insert
    :: ( PersistEntityBackend a ~ SqlBackend
       , PersistEntity a
       , Monad m
       , GraphulaSafeToInsert a
       )
    => Maybe (Key a)
    -> a
    -> m (Maybe (Entity a))
  insert mk a = insertVerbose mk a >>= \case
    Right ea -> pure (Just ea)
    Left _ -> pure Nothing

  remove
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m)
    => Key a
    -> m ()
  {-# MINIMAL remove, insert | remove, insertVerbose #-}

class MonadGraphulaBackend m where
  type Logging m :: Type -> Constraint
  askGen :: m (IORef QCGen)
  logNode :: Logging m a => a -> m ()
