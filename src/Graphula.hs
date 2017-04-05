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
  , Frontend(..)
  , HasDependencies(..)
  , nodeEditWith
  , nodeWith
  , nodeEdit
  , node
  , runGraphula
  , NoConstraint
  ) where

import Test.QuickCheck
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Exception (Exception, throwIO)
import Data.Functor.Sum
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)


type Graph constraint entity = FreeT (Sum Backend (Frontend constraint entity))

runGraphula
  :: (MonadIO m)
  => (Frontend constraint entity (m a) -> m a) -> Graph constraint entity m a -> m a
runGraphula frontend f = flip iterT f $ \case
  InR r -> frontend r
  InL l -> backendArbitrary l
    where
      backendArbitrary = \case
        GenerateNode next ->
          next =<< (liftIO . generate $ arbitrary)
        Throw e next ->
          next =<< (liftIO $ throwIO e)

liftLeft :: (Monad m, Functor f, Functor g) => FreeT f m a -> FreeT (Sum f g) m a
liftLeft = transFreeT InL

liftRight :: (Monad m, Functor f, Functor g) => FreeT g m a -> FreeT (Sum f g) m a
liftRight = transFreeT InR


data Frontend (constraint :: * -> Constraint) entity next where
  Insert :: constraint a => a -> (Maybe (entity a) -> next) -> Frontend constraint entity next

deriving instance Functor (Frontend constraint entity)

insert :: (Monad m, constraint a) => a -> Graph constraint entity m (Maybe (entity a))
insert n = liftRight $ liftF (Insert n id)


data Backend next where
  GenerateNode :: Arbitrary a => (a -> next) -> Backend next
  Throw :: Exception e => e -> (a -> next) -> Backend next

deriving instance Functor Backend

generateNode :: (Monad m, Arbitrary a) => Graph constraint entity m a
generateNode = liftLeft . liftF $ GenerateNode id

throw :: (Monad m, Exception e) => e -> Graph constraint entity m a
throw e = liftLeft . liftF $ Throw e id


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
  :: forall a entity constraint m. (Monad m, constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> (a -> a) -> Graph constraint entity m (entity a)
nodeEditWith dependencies edits =
  tryInsert 10 0 $ do
    x <- generateNode
    pure (edits x `dependsOn` dependencies)

nodeWith
  :: forall a entity constraint m. (Monad m, constraint a, Typeable a, Arbitrary a, HasDependencies a)
  => (Dependencies a) -> Graph constraint entity m (entity a)
nodeWith = flip nodeEditWith id

nodeEdit
  :: forall a entity constraint m. (Monad m, constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => (a -> a) -> Graph constraint entity m (entity a)
nodeEdit edits = nodeEditWith () edits

node
  :: forall a entity constraint m. (Monad m, constraint a, Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ())
  => Graph constraint entity m (entity a)
node = nodeWith ()

tryInsert
  :: forall a entity constraint m. (Monad m, constraint a, Typeable a)
  => Int -> Int -> (Graph constraint entity m a) -> Graph constraint entity m (entity a)
tryInsert maxAttempts currentAttempts source
  | currentAttempts >= maxAttempts =
      throw $ GenerationFailureMaxAttempts (typeRep (Proxy :: Proxy a))
  | otherwise = do
    value <- source
    insert value >>= \case
      Just a -> pure a
      Nothing -> tryInsert maxAttempts (succ currentAttempts) source