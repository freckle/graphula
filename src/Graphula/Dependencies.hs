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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Graphula.Dependencies
  ( HasDependencies(..)
  , Only(..)
  , only

  -- * Non-serial keys
  , KeySourceType(..)
  , GenerateKey
  , generateKey
  ) where

import Prelude

import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Database.Persist (Key)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Generics.Eot (Eot, HasEot, fromEot, toEot)
import Graphula.Dependencies.Generic
import Graphula.NoConstraint
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen)

class HasDependencies a where
  -- | A data type that contains values to be injected into @a@ via
  -- `dependsOn`. The default generic implementation of `dependsOn` supports
  -- tuples as 'Dependencies'. Data types with a single dependency should use
  -- 'Only' as a 1-tuple.
  --
  -- note: The contents of a tuple must be ordered as they appear in the
  -- definition of @a@.
  type Dependencies a
  type instance Dependencies _a = ()

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
  -- Most types will use @'SourceDefault'@ or @'SourceArbitrary'@. Only
  -- use @'SourceExternal'@ if the key for a value is always defined
  -- externally.
  --
  type KeySource a :: KeySourceType
  type instance KeySource _a = 'SourceDefault

  -- | Assign values from the 'Dependencies' collection to a value.
  -- 'dependsOn' must be an idempotent operation.
  --
  -- Law:
  --
  -- prop> (\x d -> x `dependsOn` d `dependsOn` d) = dependsOn
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

-- | For entities that only have singular 'Dependencies'
newtype Only a = Only { fromOnly :: a }
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

only :: a -> Only a
only = Only

data KeySourceType
  = SourceDefault
  -- ^ Generate keys using the database's @DEFAULT@ strategy
  | SourceArbitrary
  -- ^ Generate keys using the @'Arbitrary'@ instance for the @'Key'@
  | SourceExternal
  -- ^ Always explicitly pass an external key

-- | Abstract over how keys are generated using @'SourceDefault'@ or @'SourceArbitrary'@
class (GenerateKeyInternal (KeySource a) a, KeyConstraint (KeySource a) a) => GenerateKey a
instance (GenerateKeyInternal (KeySource a) a, KeyConstraint (KeySource a) a) => GenerateKey a

-- | Handle key generation for @'SourceDefault'@ and @'SourceArbitrary'@
--
-- Ths could be a single-parameter class, but carrying the @a@ around
-- lets us give a better error message when @'node'@ is called instead
-- of @'nodeKeyed'@.
--
class GenerateKeyInternal (s :: KeySourceType) a where
  type KeyConstraint s a :: Constraint
  generateKey :: KeyConstraint s a => Gen (Maybe (Key a))

instance GenerateKeyInternal 'SourceDefault a where
  type KeyConstraint 'SourceDefault a = NoConstraint a
  generateKey = pure Nothing

instance GenerateKeyInternal 'SourceArbitrary a where
  type KeyConstraint 'SourceArbitrary a = Arbitrary (Key a)
  generateKey = Just <$> arbitrary

-- | Explicit instance for @'SourceExternal'@ to give an actionable error message
--
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
instance TypeError
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
    ':<>: 'Text  " = "
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
  ) => GenerateKeyInternal 'SourceExternal a where
  type KeyConstraint 'SourceExternal a = NoConstraint a
  generateKey = error "unreachable"

type family Quote t where
  Quote t = 'Text "‘" ':<>: t ':<>: 'Text "’"
