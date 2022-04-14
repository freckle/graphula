{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Graphula.Node
  (
  -- * Generating
    node
  , nodeKeyed

  -- * 'NodeOptions'
  , NodeOptions
  , edit
  , ensure

  -- * Exceptions
  , GenerationFailure(..)
  ) where

import Prelude

import Control.Monad (guard, (<=<))
import Data.Proxy (Proxy(..))
import Data.Semigroup.Generic (gmappend, gmempty)
import Data.Traversable (for)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Database.Persist (Entity(..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)
import Graphula.Arbitrary
import Graphula.Class
import Graphula.Dependencies
import Test.QuickCheck (Arbitrary(..))
import UnliftIO.Exception (Exception, throwIO)

#if MIN_VERSION_persistent(2,14,0)
import Database.Persist.Class.PersistEntity
#endif

-- | Options for generating an individual node
--
--
-- 'NodeOptions' can be created and combined with the Monoidal operations '(<>)'
-- and 'mempty'.
--
-- > a1 <- node @A () mempty
-- > a2 <- node @A () $ edit $ \a -> a { someField = True }
-- > a3 <- node @A () $ ensure $ (== True) . someField
--
newtype NodeOptions a = NodeOptions
  { nodeOptionsEdit :: Kendo Maybe a
  }
  deriving stock Generic

instance Semigroup (NodeOptions a) where
  (<>) = gmappend
  {-# INLINE (<>) #-}

instance Monoid (NodeOptions a) where
  mempty = gmempty
  {-# INLINE mempty #-}

-- | Like @'Endo'@ but uses Kliesli composition
newtype Kendo m a = Kendo { appKendo :: a -> m a }
    deriving stock Generic

instance Monad m => Semigroup (Kendo m a) where
  Kendo f <> Kendo g = Kendo $ f <=< g
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Kendo m a) where
  mempty = Kendo pure
  {-# INLINE mempty #-}

-- | Modify the node after it's been generated
--
-- > a <- node @A () $ edit $ \a -> a { someField = True }
--
edit :: (a -> a) -> NodeOptions a
edit f = mempty { nodeOptionsEdit = Kendo $ Just . f }

-- | Require a node to satisfy the specified predicate
--
-- > a <- node @A () $ ensure $ (== True) . someField
--
-- N.B. ensuring a condition that is infrequently met can be innefficient.
--
ensure :: (a -> Bool) -> NodeOptions a
ensure f = mempty { nodeOptionsEdit = Kendo $ \a -> a <$ guard (f a) }

-- | Generate a node with a default (Database-provided) key
--
-- > a <- node @A () mempty
--
node
  :: forall a m
   . ( MonadGraphula m
     , Logging m a
     , Arbitrary a
     , HasDependencies a
     , GenerateKey a
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
     , GraphulaSafeToInsert a
     )
  => Dependencies a
  -> NodeOptions a
  -> m (Entity a)
node = nodeImpl $ generate $ generateKey @(KeySource a) @a

-- | Generate a node with an explictly-given key
--
-- > let someKey = UUID.fromString "..."
-- > a <- nodeKeyed @A someKey () mempty
--
nodeKeyed
  :: forall a m
   . ( MonadGraphula m
     , Logging m a
     , Arbitrary a
     , HasDependencies a
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
     , GraphulaSafeToInsert a
     )
  => Key a
  -> Dependencies a
  -> NodeOptions a
  -> m (Entity a)
nodeKeyed key = nodeImpl $ pure $ Just key

nodeImpl
  :: forall a m
   . ( MonadGraphula m
     , Logging m a
     , Arbitrary a
     , HasDependencies a
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
     , GraphulaSafeToInsert a
     )
  => m (Maybe (Key a))
  -> Dependencies a
  -> NodeOptions a
  -> m (Entity a)
nodeImpl genKey dependencies NodeOptions {..} = attempt 100 10 $ do
  initial <- generate arbitrary
  for (appKendo nodeOptionsEdit initial) $ \edited -> do
    -- N.B. dependencies setting always overrules edits
    let hydrated = edited `dependsOn` dependencies
    logNode hydrated
    mKey <- genKey
    pure (mKey, hydrated)

data GenerationFailure
  = GenerationFailureMaxAttemptsToConstrain TypeRep
  -- ^ Could not satisfy constraints defined using 'ensure'
  | GenerationFailureMaxAttemptsToInsert TypeRep
  -- ^ Could not satisfy database constraints on 'insert'
  deriving stock (Show, Eq)

instance Exception GenerationFailure

attempt
  :: forall a m
   . ( MonadGraphula m
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
#if MIN_VERSION_persistent(2,14,0)
     , SafeToInsert a
#endif
     )
  => Int
  -> Int
  -> m (Maybe (Maybe (Key a), a))
  -> m (Entity a)
attempt maxEdits maxInserts source = loop 0 0
 where
  loop :: Int -> Int -> m (Entity a)
  loop numEdits numInserts
    | numEdits >= maxEdits = die GenerationFailureMaxAttemptsToConstrain
    | numInserts >= maxInserts = die GenerationFailureMaxAttemptsToInsert
    | otherwise = source >>= \case
      Nothing -> loop (succ numEdits) numInserts
      --               ^ failed to edit, only increments this
      Just (mKey, value) -> insert mKey value >>= \case
        Nothing -> loop (succ numEdits) (succ numInserts)
        --               ^ failed to insert, but also increments this. Are we
        --                 sure that's what we want?
        Just a -> pure a

  die :: (TypeRep -> GenerationFailure) -> m (Entity a)
  die e = throwIO $ e $ typeRep (Proxy :: Proxy a)
