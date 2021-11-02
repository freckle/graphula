{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Machinery for the 'Generic'-based 'HasDependencies' instance
module Graphula.Dependencies.Generic
  ( GHasDependencies(..)
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Generics.Eot (Proxy(..), Void)

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
