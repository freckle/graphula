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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Graphula.Node
  ( -- * Generating
    node
  , nodeKeyed

    -- * 'NodeOptions'
  , NodeOptions
  , edit
  , ensure

    -- * Exceptions
  , GenerationFailure (..)
  ) where

import Prelude

import Control.Monad (guard, (<=<))
import Data.Proxy (Proxy (..))
import Data.Semigroup.Generic (gmappend, gmempty)
import Data.Traversable (for)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Database.Persist (Entity (..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)
import Graphula.Arbitrary
import Graphula.Class
import Graphula.Dependencies
import Test.QuickCheck (Arbitrary (..))
import UnliftIO (MonadIO)
import UnliftIO.Exception (Exception, throwIO)

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
-- The Semigroup orders the operations from right to left. For example,
-- @'edit' z <> 'ensure' y <> 'edit' x@ first performs @'edit' x@, then fails if
-- the value does not satisfy assertion @y@, then performs @'edit' z@.
newtype NodeOptions a = NodeOptions
  { nodeOptionsEdit :: Kendo Maybe a
  }
  deriving stock (Generic)

instance Semigroup (NodeOptions a) where
  (<>) = gmappend
  {-# INLINE (<>) #-}

instance Monoid (NodeOptions a) where
  mempty = gmempty
  {-# INLINE mempty #-}

-- | Like @'Endo'@ but uses Kliesli composition
newtype Kendo m a = Kendo {appKendo :: a -> m a}
  deriving stock (Generic)

instance Monad m => Semigroup (Kendo m a) where
  Kendo f <> Kendo g = Kendo $ f <=< g
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Kendo m a) where
  mempty = Kendo pure
  {-# INLINE mempty #-}

-- | Modify the node after it's been generated
--
-- > a <- node @A () $ edit $ \a -> a { someField = True }
edit :: (a -> a) -> NodeOptions a
edit f = mempty {nodeOptionsEdit = Kendo $ Just . f}

-- | Require a node to satisfy the specified predicate
--
-- > a <- node @A () $ ensure $ (== True) . someField
--
-- N.B. ensuring a condition that is infrequently met can be innefficient.
ensure :: (a -> Bool) -> NodeOptions a
ensure f = mempty {nodeOptionsEdit = Kendo $ \a -> a <$ guard (f a)}

-- | Generate a node with a default (Arbitrary or database-provided) key
--
-- > a <- node @A () mempty
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
     )
  => Dependencies a
  -> NodeOptions a
  -> m (Entity a)
node dependencies NodeOptions {..} =
  let genKey = generate $ generateKey @(KeySource a) @a
  in  attempt 100 10 $ do
        initial <- generate arbitrary
        for (appKendo nodeOptionsEdit initial) $ \edited -> do
          -- N.B. dependencies setting always overrules edits
          let hydrated = edited `dependsOn` dependencies
          logNode hydrated
          mKey <- genKey
          pure (mKey, hydrated)

attempt
  :: forall a m
   . ( MonadGraphula m
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , GenerateKey a
     , Typeable a
     )
  => Int
  -> Int
  -> m (Maybe (KeyForInsert a, a))
  -> m (Entity a)
attempt maxEdits maxInserts source = loop 0 0 Nothing
 where
  loop :: Int -> Int -> Maybe String -> m (Entity a)
  loop numEdits numInserts mmsg
    | numEdits >= maxEdits = die $ GenerationFailureMaxAttemptsToConstrain mmsg
    | numInserts >= maxInserts = die $ GenerationFailureMaxAttemptsToInsert mmsg
    | otherwise =
        source >>= \case
          Nothing -> loop (succ numEdits) numInserts Nothing
          --               ^ failed to edit, only increments this
          Just (mKey, value) ->
            insertWithPossiblyRequiredKeyEither mKey value >>= \case
              Left msg -> loop (succ numEdits) (succ numInserts) $ Just msg
              Right a -> pure a

-- | Generate a node with an explictly-given key
--
-- > let someKey = UUID.fromString "..."
-- > a <- nodeKeyed @A someKey () mempty
nodeKeyed
  :: forall a m
   . ( MonadGraphula m
     , Logging m a
     , Arbitrary a
     , HasDependencies a
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
     )
  => Key a
  -> Dependencies a
  -> NodeOptions a
  -> m (Entity a)
nodeKeyed key dependencies NodeOptions {..} =
  attempt' 100 10 key $ do
    initial <- generate arbitrary
    for (appKendo nodeOptionsEdit initial) $ \edited -> do
      -- N.B. dependencies setting always overrules edits
      let hydrated = edited `dependsOn` dependencies
      logNode hydrated
      pure hydrated

attempt'
  :: forall a m
   . ( MonadGraphula m
     , PersistEntityBackend a ~ SqlBackend
     , PersistEntity a
     , Typeable a
     )
  => Int
  -> Int
  -> Key a
  -> m (Maybe a)
  -> m (Entity a)
attempt' maxEdits maxInserts key source = loop 0 0 Nothing
 where
  loop :: Int -> Int -> Maybe String -> m (Entity a)
  loop numEdits numInserts mmsg
    | numEdits >= maxEdits = die $ GenerationFailureMaxAttemptsToConstrain mmsg
    | numInserts >= maxInserts = die $ GenerationFailureMaxAttemptsToInsert mmsg
    | otherwise =
        source >>= \case
          Nothing -> loop (succ numEdits) numInserts Nothing
          --               ^ failed to edit, only increments this
          Just value ->
            insertKeyedEither key value >>= \case
              Left msg -> loop (succ numEdits) (succ numInserts) $ Just msg
              Right a -> pure a

die
  :: forall a m
   . (MonadIO m, Typeable a)
  => (TypeRep -> GenerationFailure)
  -> m (Entity a)
die e = throwIO $ e $ typeRep $ Proxy @a

data GenerationFailure
  = -- | Could not satisfy constraints defined using 'ensure'
    GenerationFailureMaxAttemptsToConstrain (Maybe String) TypeRep
  | -- | Could not satisfy database constraints on 'insert'
    GenerationFailureMaxAttemptsToInsert (Maybe String) TypeRep
  deriving stock (Show, Eq)

instance Exception GenerationFailure
