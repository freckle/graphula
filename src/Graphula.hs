{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphula
  ( module Graphula
  , Arbitrary(..)
  ) where

import Test.QuickCheck
import Control.Monad.Free
import Control.Exception
import Data.Proxy
import Data.Typeable


type Graph entity = Free (Actions entity)


data Actions entity next where
  Insert :: a -> (Maybe (entity a) -> next) -> Actions entity next
  Validate :: a -> (Bool -> next) -> Actions entity next
  LiftIO :: IO a -> (a -> next) -> Actions entity next

deriving instance Functor (Actions entity)

liftIO :: IO a -> Graph entity a
liftIO io = liftF (LiftIO io id)

insert :: a -> Graph entity (Maybe (entity a))
insert n = liftF (Insert n id)

validate :: a -> Graph entity Bool
validate n = liftF (Validate n id)


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

nodeEditWith :: (Typeable a, Arbitrary a, HasDependencies a) => (Dependencies a) -> (a -> a) -> Graph entity (entity a)
nodeEditWith dependencies edits =
  tryInsert 10 0 $ do
    x <- liftIO $ generate arbitrary
    pure (edits x `dependsOn` dependencies)

nodeWith :: (Typeable a, Arbitrary a, HasDependencies a) => (Dependencies a) -> Graph entity (entity a)
nodeWith = flip nodeEditWith id

nodeEdit :: (Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ()) => (a -> a) -> Graph entity (entity a)
nodeEdit edits = nodeEditWith () edits

node :: (Typeable a, Arbitrary a, HasDependencies a, Dependencies a ~ ()) => Graph entity (entity a)
node = nodeWith ()

tryInsert :: forall a entity. Typeable a => Int -> Int -> (Graph entity a) -> Graph entity (entity a)
tryInsert maxAttempts currentAttempts source
  | currentAttempts >= maxAttempts =
      liftIO . throwIO $ GenerationFailureMaxAttempts (typeRep (Proxy :: Proxy a))
  | otherwise = do
    value <- source
    isValid <- validate value
    if isValid then
      insert value >>= \case
        Just a -> pure a
        Nothing -> tryInsert maxAttempts (succ currentAttempts) source
    else
      tryInsert maxAttempts (succ currentAttempts) source
