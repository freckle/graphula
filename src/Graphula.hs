{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphula
  ( Graph
  , Actions(..)
  , HasDependencies(..)
  , nodeEditWith
  , nodeWith
  , nodeEdit
  , node
  , NoConstraint
  ) where

import Test.QuickCheck
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Exception
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)


type Graph constraint entity = FreeT (Actions constraint entity)

data Actions (constraint :: * -> Constraint) entity next where
  Insert :: constraint a => a -> (Maybe (entity a) -> next) -> Actions constraint entity next

deriving instance Functor (Actions constraint entity)

insert :: (Monad m, constraint a) => a -> Graph constraint entity m (Maybe (entity a))
insert n = liftF (Insert n id)


class NoConstraint a where

instance NoConstraint a where


class HasDependencies a where
  type Dependencies a
  -- defaults to having no dependencies
  type instance Dependencies a = ()
  dependsOn :: a -> Dependencies a -> a
  dependsOn = const


data GenerationFailure =
  GenerationFailureMaxAttempts TypeRep
  deriving (Show, Typeable)

instance Exception GenerationFailure

nodeEditWith
  :: forall a entity constraint m. (MonadIO m, constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> (a -> a) -> Graph constraint entity m (entity a)
nodeEditWith dependencies edits =
  tryInsert 10 0 $ do
    x <- liftIO $ generate arbitrary
    pure (edits x `dependsOn` dependencies)

nodeWith
  :: forall a entity constraint m. (MonadIO m, constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> Graph constraint entity m (entity a)
nodeWith = flip nodeEditWith id

nodeEdit
  :: forall a entity constraint m. (MonadIO m, constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => (a -> a) -> Graph constraint entity m (entity a)
nodeEdit edits = nodeEditWith () edits

node
  :: forall a entity constraint m. (MonadIO m, constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => Graph constraint entity m (entity a)
node = nodeWith ()

tryInsert
  :: forall a entity constraint m. (MonadIO m, constraint a, Typeable a)
  => Int -> Int -> (Graph constraint entity m a) -> Graph constraint entity m (entity a)
tryInsert maxAttempts currentAttempts source
  | currentAttempts >= maxAttempts =
      liftIO . throwIO $ GenerationFailureMaxAttempts (typeRep (Proxy :: Proxy a))
  | otherwise = do
    value <- source
    insert value >>= \case
      Just a -> pure a
      Nothing -> tryInsert maxAttempts (succ currentAttempts) source
