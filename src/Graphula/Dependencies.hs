{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Graphula.Dependencies
  ( HasDependencies (..)
  , Only (..)
  , only

    -- * Non-serial keys
  , KeySourceType (..)
  , KeySourceTypeM
  , KeyForInsert
  , KeyRequirementForInsert
  , InsertWithPossiblyRequiredKey (..)
  , Required (..)
  , Optional (..)
  , GenerateKey
  , generateKey
  ) where

import Prelude

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Database.Persist (Entity (..), Key, PersistEntity, PersistEntityBackend)
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Generics.Eot (Eot, HasEot, fromEot, toEot)
import Graphula.Class (GraphulaSafeToInsert, MonadGraphulaFrontend)
import qualified Graphula.Class as MonadGraphulaFrontend
  ( MonadGraphulaFrontend (..)
  )
import Graphula.Dependencies.Generic
import Graphula.NoConstraint
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen)

class HasDependencies a where
  -- | A data type declaring the model's dependencies
  --
  -- Models with no dependencies can declare an empty instance,
  --
  -- @
  -- instance 'HasDependencies' School
  -- @
  --
  -- Models with one dependency must use the 'Only' 1-tuple constructor,
  --
  -- @
  -- instance 'HasDependencies' Teacher where
  --   type Dependencies Teacher = Only SchoolId
  -- @
  --
  -- Models with multiple dependencies use tuple syntax,
  --
  -- @
  -- instance 'HasDependencies' Course where
  --   type Dependencies Course = (SchoolId, TeacherId)
  -- @
  type Dependencies a

  type Dependencies _a = ()

  -- | Specify the method for resolving a node's key
  --
  -- This can be
  --
  -- @
  -- 'SourceDefault   -- automatically generate keys from the database
  -- 'SourceArbitrary -- automatically generate keys using @'Arbitrary'@
  -- 'SourceExternal  -- explicitly pass a key using @'nodeKeyed'@
  -- @
  --
  -- Most types will use 'SourceDefault' or 'SourceArbitrary'. Only use
  -- 'SourceExternal' if the key for a value is always defined externally.
  type KeySource a :: KeySourceType

  type KeySource _a = 'SourceDefault

  -- | Assign values from the 'Dependencies' collection to a value
  --
  -- This must be an idempotent operation. Law:
  --
  -- prop> (\x d -> x `dependsOn` d `dependsOn` d) = dependsOn
  --
  -- The default, 'Generic'-based implementation will assign values by the order
  -- of the fields in the model's type.
  dependsOn :: a -> Dependencies a -> a
  default dependsOn
    :: ( HasEot a
       , HasEot (Dependencies a)
       , GHasDependencies
          (Proxy a)
          (Proxy (Dependencies a))
          (Eot a)
          (Eot (Dependencies a))
       )
    => a
    -> Dependencies a
    -> a
  dependsOn a dependencies =
    fromEot $
      genericDependsOn
        (Proxy :: Proxy a)
        (Proxy :: Proxy (Dependencies a))
        (toEot a)
        (toEot dependencies)

-- | For entities that only have singular 'Dependencies'
newtype Only a = Only {fromOnly :: a}
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

type role Only representational

only :: a -> Only a
only = Only

data KeySourceType
  = -- | Generate keys using the database's @DEFAULT@ strategy
    SourceDefault
  | -- | Generate keys using the 'Arbitrary' instance for the 'Key'
    SourceArbitrary
  | -- | Always explicitly pass an external key
    --
    -- See 'nodeKeyed'.
    SourceExternal

newtype Required a = Required a

type role Required representational

newtype Optional a = Optional (Maybe a)

type role Optional representational

-- | When a user of Graphula inserts, this wraps the key they provide.
--   For 'SourceExternal' a key is required; for others it's optional.
type family KeySourceTypeM (t :: KeySourceType) :: Type -> Type where
  KeySourceTypeM 'SourceExternal = Required
  KeySourceTypeM _ = Optional

type KeyRequirementForInsert record = KeySourceTypeM (KeySource record)

-- | When Graphula inserts into Persistent, this wraps the key is provides.
--   For 'SourceDefault', a key is optional; for others it has always been
--   generated.
type family KeySourceTypeInternalM (t :: KeySourceType) :: Type -> Type where
  KeySourceTypeInternalM 'SourceDefault = Optional
  KeySourceTypeInternalM _ = Required

type KeyRequirementForInsertInternal record =
  KeySourceTypeInternalM (KeySource record)

-- | When Graphula inserts into Persistent, this is the record's key.
type KeyForInsert record = KeyRequirementForInsertInternal record (Key record)

class InsertWithPossiblyRequiredKey (requirement :: Type -> Type) where
  type InsertConstraint requirement :: Type -> Constraint
  insertWithPossiblyRequiredKey
    :: ( PersistEntityBackend record ~ SqlBackend
       , PersistEntity record
       , Monad m
       , MonadGraphulaFrontend m
       , InsertConstraint requirement record
       )
    => requirement (Key record)
    -> record
    -> m (Maybe (Entity record))
  justKey :: key -> requirement key

instance InsertWithPossiblyRequiredKey Optional where
  type InsertConstraint Optional = GraphulaSafeToInsert
  insertWithPossiblyRequiredKey (Optional key) = MonadGraphulaFrontend.insert key
  justKey = Optional . Just

instance InsertWithPossiblyRequiredKey Required where
  type InsertConstraint Required = NoConstraint
  insertWithPossiblyRequiredKey (Required key) = MonadGraphulaFrontend.insertKeyed key
  justKey = Required

-- | Abstract constraint that some @a@ can generate a key
--
-- This is part of ensuring better error messages.
class
  ( GenerateKeyInternal (KeySource a) a
  , KeyConstraint (KeySource a) a
  , InsertWithPossiblyRequiredKey (KeySourceTypeInternalM (KeySource a))
  , InsertConstraint (KeySourceTypeInternalM (KeySource a)) a
  ) =>
  GenerateKey a

instance
  ( GenerateKeyInternal (KeySource a) a
  , KeyConstraint (KeySource a) a
  , InsertWithPossiblyRequiredKey (KeySourceTypeInternalM (KeySource a))
  , InsertConstraint (KeySourceTypeInternalM (KeySource a)) a
  )
  => GenerateKey a

class GenerateKeyInternal (s :: KeySourceType) a where
  type KeyConstraint s a :: Constraint
  generateKey :: KeyConstraint s a => Gen (KeySourceTypeInternalM s (Key a))

instance GenerateKeyInternal 'SourceDefault a where
  type KeyConstraint 'SourceDefault a = GraphulaSafeToInsert a
  generateKey = pure (Optional Nothing)

instance GenerateKeyInternal 'SourceArbitrary a where
  type KeyConstraint 'SourceArbitrary a = Arbitrary (Key a)
  generateKey = Required <$> arbitrary

-- Rendered:
--
-- @
-- Cannot generate a value of type ‘X’ using ‘node’ since
--
--   instance HasDependencies X where
--     type KeySource X = 'SourceExternal
--
-- Possible fixes include:
-- • Use ‘nodeKeyed’ instead of ‘node’
-- • Change ‘KeySource X’ to 'SourceDefault or 'SourceArbitrary
-- @
--
instance
  TypeError
    ( 'Text "Cannot generate a value of type "
        ':<>: Quote ('ShowType a)
        ':<>: 'Text " using "
        ':<>: Quote ('Text "node")
        ':<>: 'Text " since"
        ':$$: 'Text ""
        ':$$: 'Text "  instance HasDependencies "
        ':<>: 'ShowType a
        ':<>: 'Text " where"
        ':$$: 'Text "    "
        ':<>: 'Text "type KeySource "
        ':<>: 'ShowType a
        ':<>: 'Text " = "
        ':<>: 'ShowType 'SourceExternal
        ':$$: 'Text ""
        ':$$: 'Text "Possible fixes include:"
        ':$$: 'Text "• Use "
        ':<>: Quote ('Text "nodeKeyed")
        ':<>: 'Text " instead of "
        ':<>: Quote ('Text "node")
        ':$$: 'Text "• Change "
        ':<>: Quote ('Text "KeySource " ':<>: 'ShowType a)
        ':<>: 'Text " to "
        ':<>: 'Text "'SourceDefault"
        ':<>: 'Text " or "
        ':<>: 'Text "'SourceArbitrary"
    )
  => GenerateKeyInternal 'SourceExternal a
  where
  type KeyConstraint 'SourceExternal a = NoConstraint a
  generateKey = error "unreachable"

type family Quote t where
  Quote t = 'Text "‘" ':<>: t ':<>: 'Text "’"
