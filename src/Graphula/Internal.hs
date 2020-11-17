{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Graphula.Internal where

import Data.IORef (IORef)
import Data.Kind (Constraint, Type)
import Database.Persist (Key)
import Generics.Eot (Proxy(..), Void)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Test.QuickCheck (Arbitrary(..), Gen)
import Test.QuickCheck.Random (QCGen)

class MonadGraphulaBackend m where
  type Logging m :: Type -> Constraint
  -- ^ A constraint provided to log details of the graph to some form of
  --   persistence. This is used by 'runGraphulaLogged' to store graph nodes as
  --   'Show'n 'Text' values
  askGen :: m (IORef QCGen)
  logNode :: Logging m a => a -> m ()

data Match t
  = NoMatch t
  | Match t

type family DependenciesTypeInstance nodeTy depsTy where
  DependenciesTypeInstance nodeTy depsTy =
    'Text "‘type Dependencies " ':<>: 'ShowType nodeTy ':<>:
    'Text " = " ':<>: 'ShowType depsTy ':<>: 'Text "’"

-- Walk through the fields of our node and match them up with fields from the dependencies.
type family FindMatches nodeTy depsTy as ds :: [Match Type] where
  -- Excess dependencies
  FindMatches nodeTy depsTy () (d, _ds) =
    TypeError
      ( 'Text "Excess dependency ‘" ':<>: 'ShowType d ':<>:
        'Text "’ in " ':$$: DependenciesTypeInstance nodeTy depsTy ':$$:
        'Text "Ordering of dependencies must match their occurrence in the target type ‘" ':<>:
        'ShowType nodeTy ':<>: 'Text "’"
      )

  -- No more fields or dependencies left
  FindMatches _nodeTy _depsTy () () = '[]

  -- Fields left, but no more dependencies
  FindMatches nodeTy depsTy (a, as) () = 'NoMatch a ': FindMatches nodeTy depsTy as ()

  -- Field matches dependency, keep going
  FindMatches nodeTy depsTy (a, as) (a, ds) = 'Match a ': FindMatches nodeTy depsTy as ds

  -- Field does not match dependency, keep going
  FindMatches nodeTy depsTy (a, as) (d, ds) = 'NoMatch a ': FindMatches nodeTy depsTy as (d, ds)

class GHasDependencies nodeTyProxy depsTyProxy node deps where
  genericDependsOn :: nodeTyProxy -> depsTyProxy -> node -> deps -> node

class GHasDependenciesRecursive fieldsProxy node deps where
  genericDependsOnRecursive :: fieldsProxy -> node -> deps -> node

-- This instance head only matches EoT representations of
-- datatypes with no constructors and no dependencies
instance {-# OVERLAPPING #-} GHasDependencies (Proxy nodeTy) (Proxy depsTy) Void (Either () Void) where
  genericDependsOn _ _ node _ = node

-- This instance warns the user if they give dependencies
-- to a datatype with no constructors
instance
  {-# OVERLAPPABLE #-}
  ( TypeError
    ( 'Text "A datatype with no constructors can't use the dependencies in" ':$$:
      DependenciesTypeInstance nodeTy depsTy
    )
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) Void (Either deps rest) where
  genericDependsOn _ _ _ _ = error "Impossible"

-- This instance head only matches EoT representations of
-- datatypes with a single constructor
instance
  ( FindMatches nodeTy depsTy node deps ~ fields
  , GHasDependenciesRecursive (Proxy fields) node deps
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) (Either node Void) (Either deps Void) where
  genericDependsOn _ _ (Left node) (Left deps) =
    Left (genericDependsOnRecursive (Proxy :: Proxy fields) node deps)
  genericDependsOn _ _ _ _ = error "Impossible" -- EoT never generates an actual `Right (x :: Void)` here

-- This instance matches a sum type as both node and dependencies.
-- We use this to report an error to the user.
instance
  ( TypeError
    ( 'Text "Cannot automatically find dependencies for sum type in" ':$$:
      DependenciesTypeInstance nodeTy depsTy
    )
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) (Either left (Either right rest)) (Either deps Void) where
  genericDependsOn _ _ _ _ = error "Impossible"

-- This instance matches a sum type as the node.
-- This is also an error.
instance
  ( TypeError
    ( 'Text "Cannot automatically use a sum type as dependencies in" ':$$:
      DependenciesTypeInstance nodeTy depsTy
    )
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) (Either node Void) (Either left (Either right rest)) where
  genericDependsOn _ _ _ _ = error "Impossible"

-- This instance matches a sum type as the dependencies.
-- This is also an error.
instance
  ( TypeError
    ( 'Text "Cannot automatically find dependencies for sum type or use a sum type as a dependency in" ':$$:
      DependenciesTypeInstance nodeTy depsTy
    )
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) (Either left1 (Either right1 rest1)) (Either left2 (Either right2 rest2)) where
  genericDependsOn _ _ _ _ = error "Impossible"

-- Don't let the user specify `Void` as a dependency
instance
  ( TypeError
    ( 'Text "Use ‘()’ instead of ‘Void’ for datatypes with no dependencies in" ':$$:
      DependenciesTypeInstance nodeTy depsTy
    )
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) node Void where
  genericDependsOn _ _ _ _ = error "Impossible"

instance
  ( a ~ dep
  , GHasDependenciesRecursive (Proxy fields) as deps
  ) => GHasDependenciesRecursive (Proxy ('Match a ': fields)) (a, as) (dep, deps) where
  genericDependsOnRecursive _ (_, as) (dep, deps) =
    (dep, genericDependsOnRecursive (Proxy :: Proxy fields) as deps)

instance
  ( GHasDependenciesRecursive (Proxy fields) as deps
  ) => GHasDependenciesRecursive (Proxy ('NoMatch a ': fields)) (a, as) deps where
  genericDependsOnRecursive _ (a, as) deps =
    (a, genericDependsOnRecursive (Proxy :: Proxy fields) as deps)

-- Without the kind-signature for '[], ghc will fail to find this
-- instance for nullary constructors
instance GHasDependenciesRecursive (Proxy ('[] :: [Match Type])) () () where
  genericDependsOnRecursive _ _ _ = ()

data KeySourceType
  = SourceDefault
  -- ^ Generate keys using the database's @DEFAULT@ strategy
  | SourceArbitrary
  -- ^ Generate keys using the @'Arbitrary'@ instance for the @'Key'@
  | SourceExternal
  -- ^ Always explicitly pass an external key

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

-- | Graphula accepts constraints for various uses. Frontends do not always
-- utilize these constraints. 'NoConstraint' is a universal class that all
-- types inhabit. It has no behavior and no additional constraints.
class NoConstraint a
instance NoConstraint a
