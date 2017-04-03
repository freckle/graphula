{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphula
  ( module Graphula
  , Arbitrary(..)
  ) where

import Test.QuickCheck
import Control.Monad.Free
import Control.Exception
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)


type Graph constraint entity = Free (Actions constraint entity)

data Actions (constraint :: * -> Constraint) entity next where
  Insert :: constraint a => a -> (Maybe (entity a) -> next) -> Actions constraint entity next
  LiftIO :: IO a -> (a -> next) -> Actions constraint entity next

deriving instance Functor (Actions constraint entity)

liftIO :: IO a -> Graph constraint entity a
liftIO io = liftF (LiftIO io id)

insert :: constraint a => a -> Graph constraint entity (Maybe (entity a))
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
  :: forall a entity constraint. (constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> (a -> a) -> Graph constraint entity (entity a)
nodeEditWith dependencies edits =
  tryInsert 10 0 $ do
    x <- liftIO $ generate arbitrary
    pure (edits x `dependsOn` dependencies)

nodeWith
  :: forall a entity constraint. (constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> Graph constraint entity (entity a)
nodeWith = flip nodeEditWith id

nodeEdit
  :: forall a entity constraint. (constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => (a -> a) -> Graph constraint entity (entity a)
nodeEdit edits = nodeEditWith () edits

node
  :: forall a entity constraint. (constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => Graph constraint entity (entity a)
node = nodeWith ()

tryInsert
  :: forall a entity constraint. (constraint a, Typeable a)
  => Int -> Int -> (Graph constraint entity a) -> Graph constraint entity (entity a)
tryInsert maxAttempts currentAttempts source
  | currentAttempts >= maxAttempts =
      liftIO . throwIO $ GenerationFailureMaxAttempts (typeRep (Proxy :: Proxy a))
  | otherwise = do
    value <- source
    insert value >>= \case
      Just a -> pure a
      Nothing -> tryInsert maxAttempts (succ currentAttempts) source
