{-|
  Graphula is a compact interface for generating data and linking its
  dependencies. You can use this interface to generate fixtures for automated
  testing.

  The interface is extensible and supports pluggable front-ends.

  @
  runGraphula graphIdentity $ do
    -- Compose dependencies at the value level
    Identity vet <- node @Veterinarian
    Identity owner <- nodeWith @Owner $ only vet
    -- TypeApplications is not necessary, but recommended for clarity.
    Identity dog <- nodeEditWith @Dog (owner, vet) $ \d ->
      d { name = "fido" }
  @
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Graphula
  ( -- * Graph Declaration
    node
  , nodeEdit
  , nodeWith
  , nodeEditWith
  -- * Declaring Dependencies
  , HasDependencies(..)
  -- ** Singular Dependencies
  , Only(..)
  , only
  -- * The Graph Monad
  , MonadGraphulaFrontend(..)
  , MonadGraphulaBackend(..)
  -- * Extras
  , NoConstraint
  -- * Exceptions
  , GenerationFailure(..)
  ) where

import Prelude hiding (readFile, lines)
import Test.QuickCheck (Arbitrary(..), generate)
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(..), formatFailureReason)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..), MonadMask(..), bracket)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT, iterT, liftF, transFreeT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (Exception, SomeException)
import Data.Aeson (ToJSON, FromJSON, Value, Result(..), toJSON, fromJSON, encode, eitherDecodeStrict')
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq, ViewL(..), viewl, empty, (|>))
import Data.Typeable (Typeable, TypeRep, typeRep)
import Generics.Eot (fromEot, toEot, Eot, HasEot)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import System.IO (hClose, openFile, IOMode(..), Handle)
import System.IO.Temp (openTempFile)
import System.Directory (getTemporaryDirectory)

import Graphula.Internal

class MonadGraphulaFrontend m where
  type NodeConstraint m :: * -> Constraint
  -- ^ A constraint applied to nodes. This is utilized during
  --   insertion and can be leveraged by frontends with typeclass interfaces
  --   to insertion.
  type Node m :: * -> *
  -- ^ A wrapper type used to return relevant information about a given
  --   node. `graphula-persistent` returns all nodes in the 'Database.Persist.Entity' type.
  insert :: (Monad m, NodeConstraint m a) => a -> m (Maybe (Node m a))
  remove :: (Monad m, NodeConstraint m a) => Node m a -> m ()

class MonadGraphulaBackend m where
  type Logging m :: * -> Constraint
  -- ^ A constraint provided to log details of the graph to some form of
  --   persistence. This is used by 'runGraphulaLogged' to store graph nodes as
  --   JSON 'Value's.
  type Generate m :: * -> Constraint
  -- ^ A constraint for pluggable node generation. 'runGraphula'
  --   utilizes 'Arbitrary', 'runGraphulaReplay' utilizes 'FromJSON'.
  generateNode :: Generate m a => m a
  logNode :: Logging m a => a -> m ()

-- | `Graph` accepts constraints for various uses. Frontends do not always
-- utilize these constraints. 'NoConstraint' is a universal class that all
-- types inhabit. It has no behavior and no additional constraints.
class NoConstraint a where

instance NoConstraint a where

class HasDependencies a where
  -- | A data type that contains values to be injected into @a@ via
  -- `dependsOn`. The default generic implementation of `dependsOn` supports
  -- tuples as 'Dependencies'. Data types with a single dependency should use
  -- 'Only' as a 1-tuple.
  --
  -- note: The contents of a tuple must be ordered as they appear in the
  -- definition of @a@.
  type Dependencies a
  type instance Dependencies a = ()

  -- | Assign values from the 'Dependencies' collection to a value.
  -- 'dependsOn' must be an idempotent operation.
  --
  -- Law:
  --
  -- prop> dependsOn . dependsOn = dependsOn
  dependsOn :: a -> Dependencies a -> a
  default dependsOn
    ::
      ( HasEot a
      , HasEot (Dependencies a)
      , GHasDependencies (Proxy a) (Proxy (Dependencies a)) (Eot a) (Eot (Dependencies a))
      )
    => a -> Dependencies a -> a
  dependsOn a dependencies =
    fromEot $
      genericDependsOn
        (Proxy :: Proxy a)
        (Proxy :: Proxy (Dependencies a))
        (toEot a)
        (toEot dependencies)

data GenerationFailure =
  GenerationFailureMaxAttempts TypeRep
  deriving (Show, Typeable, Eq)

instance Exception GenerationFailure

{-|
  Generate and edit a value with data dependencies. This leverages
  'HasDependencies' to insert the specified data in the generated value. All
  dependency data is inserted after editing operations.

  > nodeEdit @Dog (ownerId, veterinarianId) $ \dog ->
  >   dog {name = "fido"}
-}
nodeEditWith
  :: forall a m
   . (Monad m
     , NodeConstraint m a
     , Typeable a
     , Generate m a
     , Logging m a
     , HasDependencies a
     , MonadGraphulaFrontend m
     , MonadGraphulaBackend m
     , MonadThrow m
     )
  => Dependencies a -> (a -> a) -> m (Node m a)
nodeEditWith dependencies edits =
  10 `attemptsToInsertWith` do
    x <- (`dependsOn` dependencies) . edits <$> generateNode
    logNode x
    pure x

{-|
  Generate a value with data dependencies. This leverages 'HasDependencies' to
  insert the specified data in the generated value.

  > nodeEdit @Dog (ownerId, veterinarianId)
-}
nodeWith
  :: forall a m
   . (Monad m
     , NodeConstraint m a
     , Typeable a
     , Generate m a
     , Logging m a
     , HasDependencies a
     , MonadGraphulaFrontend m
     , MonadGraphulaBackend m
     , MonadThrow m
     )
  => Dependencies a -> m (Node m a)
nodeWith = flip nodeEditWith id

{-|
  Generate and edit a value that does not have any dependencies.

  > nodeEdit @Dog $ \dog -> dog {name = "fido"}
-}
nodeEdit
  :: forall a m
   . ( Monad m
     , NodeConstraint m a
     , Typeable a
     , Generate m a
     , Logging m a
     , HasDependencies a
     , Dependencies a ~ ()
     , MonadGraphulaFrontend m
     , MonadGraphulaBackend m
     , MonadThrow m
     )
  => (a -> a) -> m (Node m a)
nodeEdit = nodeEditWith ()

-- | Generate a value that does not have any dependencies
--
-- > node @Dog
node
  :: forall a m
   . ( Monad m
     , NodeConstraint m a
     , Typeable a
     , Generate m a
     , Logging m a
     , HasDependencies a
     , Dependencies a ~ ()
     , MonadGraphulaFrontend m
     , MonadGraphulaBackend m
     , MonadThrow m
     )
  => m (Node m a)
node = nodeWith ()

attemptsToInsertWith
  :: forall a m
    . ( Monad m
      , NodeConstraint m a
      , Typeable a
      , MonadGraphulaFrontend m
      , MonadGraphulaBackend m
      , MonadThrow m
      )
  => Int
  -> m a
  -> m (Node m a)
attemptsToInsertWith attempts source
  | 0 >= attempts =
      throwM . GenerationFailureMaxAttempts $ typeRep (Proxy :: Proxy a)
  | otherwise = do
    value <- source
    insert value >>= \case
      Just a -> pure a
      Nothing -> pred attempts `attemptsToInsertWith` source

-- | For entities that only have singular 'Dependencies'. It uses data instead
-- of newtype to match laziness of builtin tuples.
data Only a = Only { fromOnly :: a }
  deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

only :: a -> Only a
only = Only
