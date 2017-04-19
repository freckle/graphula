{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphula.Internal where

import Data.Proxy (Proxy(..))
import Generics.Eot (Void)
import GHC.TypeLits (TypeError, ErrorMessage(..))

data Match t
  = NoMatch t
  | Match t

-- This looks over-specified, but if we can tolerate some overlap in the type-family,
-- then we can avoid overlap in the typeclasses below.
data List a
  = None
  | Last a
  | Cons a (List a)

type family DependenciesTypeInstance nodeTy depsTy where
  DependenciesTypeInstance nodeTy depsTy =
    'Text "‘type Dependencies " ':<>: 'ShowType nodeTy ':<>:
    'Text " = " ':<>: 'ShowType depsTy ':<>: 'Text "’"

-- Walk through the fields of our node and match them up with fields from the dependencies.
type family FindMatches nodeTy depsTy as ds where
  -- Excess dependencies
  FindMatches nodeTy depsTy () (d, ds) =
    TypeError
      ( 'Text "Excess dependency ‘" ':<>: 'ShowType d ':<>:
        'Text "’ in " ':$$: DependenciesTypeInstance nodeTy depsTy ':$$:
        'Text "Ordering of dependencies must match their occurrence in the target type ‘" ':<>:
        'ShowType nodeTy ':<>: 'Text "’"
      )

  -- No dependencies
  FindMatches nodeTy depsTy () () = 'None

  -- Last non-match
  FindMatches nodeTy depsTy (a, ()) () = 'Last ('NoMatch a)

  -- Only non-matches left
  FindMatches nodeTy depsTy (a, as) () = 'Cons ('NoMatch a) (FindMatches nodeTy depsTy as ())

  -- Last match
  FindMatches nodeTy depsTy (a, ()) (a, ()) = 'Last ('Match a)

  -- Match in the middle
  -- If we wanted, we could require the match to be on `Key a` instead of `a`
  FindMatches nodeTy depsTy (a, as) (a, ds) = 'Cons ('Match a) (FindMatches nodeTy depsTy as ds)

  -- Non-match in the middle
  FindMatches nodeTy depsTy (a, as) (d, ds) = 'Cons ('NoMatch a) (FindMatches nodeTy depsTy as (d, ds))

class GHasDependencies nodeTyProxy depsTyProxy node deps where
  genericDependsOn :: nodeTyProxy -> depsTyProxy -> node -> deps -> node

class GHasDependenciesRecursive fieldsProxy node deps where
  genericDependsOnRecursive :: fieldsProxy -> node -> deps -> node

-- This instance head only matches EoT representations of
-- datatypes with a single constructor
instance
  ( FindMatches nodeTy depsTy node deps ~ fields
  , GHasDependenciesRecursive (Proxy fields) node deps
  ) => GHasDependencies (Proxy nodeTy) (Proxy depsTy) (Either node Void) (Either deps Void) where
  genericDependsOn _ _ (Left node) (Left deps) = Left (genericDependsOnRecursive (Proxy :: Proxy fields) node deps)
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

instance
  ( a ~ dep
  , GHasDependenciesRecursive (Proxy fields) as deps
  ) => GHasDependenciesRecursive (Proxy ('Cons ('Match a) fields)) (a, as) (dep, deps) where
  genericDependsOnRecursive _ (_, as) (dep, deps) =
    (dep, genericDependsOnRecursive (Proxy :: Proxy fields) as deps)

instance
  ( GHasDependenciesRecursive (Proxy fields) as deps
  ) => GHasDependenciesRecursive (Proxy ('Cons ('NoMatch a) fields)) (a, as) deps where
  genericDependsOnRecursive _ (a, as) deps =
    (a, genericDependsOnRecursive (Proxy :: Proxy fields) as deps)

instance GHasDependenciesRecursive (Proxy ('Last ('NoMatch a))) (a, ()) () where
  genericDependsOnRecursive _ (a, ()) () = (a, ())

instance (a ~ dep) => GHasDependenciesRecursive (Proxy ('Last ('Match a))) (a, ()) (dep, ()) where
  genericDependsOnRecursive _ (_, ()) (dep, ()) = (dep, ())

instance GHasDependenciesRecursive (Proxy 'None) () () where
  genericDependsOnRecursive _ () () = ()
