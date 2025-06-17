{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Convenience functions for working with 'Key' dependencies
module Graphula.Key
  ( EntityKeys(..)
  , onlyKey
  ) where

import Database.Persist
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Graphula (Only (..), only)

class EntityKeys a where
  -- | Type-class for turning a tuple of 'Entity' into a tuple of 'Key'
  --
  -- For example, given:
  --
  -- @
  -- instance 'HasDependencies' Course where
  --   type Dependencies Course = (SchoolId, TeacherId)
  -- @
  --
  -- You would have to do,
  --
  -- @
  -- course <- 'node' @Course (entityKey school, entityKey teacher) mempty
  -- @
  --
  -- This type-class allows you to do:
  --
  -- @
  -- course <- 'node' @Course ('keys' (school, teacher)) mempty
  -- @
  --
  -- The type class instances currently scale up 4-tuple 'Dependencies'.
  type Keys a

  keys :: a -> Keys a

instance
  TypeError
    ( 'Text "Cannot use naked ‘"
        ':<>: 'ShowType (Entity a)
        ':<>: 'Text "’ as argument to ‘keys’."
        ':$$: 'Text "Did you mean ‘Only ("
        ':<>: 'ShowType (Entity a)
        ':<>: 'Text ")’?"
    )
  => EntityKeys (Entity a)
  where
  type Keys (Entity a) = Key a
  keys = entityKey

-- | Equivalent to @'Only' . 'entityKey'@
onlyKey :: Entity a -> Only (Key a)
onlyKey = keys . only

instance EntityKeys (Only (Entity a)) where
  type Keys (Only (Entity a)) = Only (Key a)
  keys (Only a) = Only (entityKey a)

instance EntityKeys (Entity a, Entity b) where
  type Keys (Entity a, Entity b) = (Key a, Key b)
  keys (a, b) = (entityKey a, entityKey b)

instance EntityKeys (Entity a, Entity b, Entity c) where
  type Keys (Entity a, Entity b, Entity c) = (Key a, Key b, Key c)
  keys (a, b, c) = (entityKey a, entityKey b, entityKey c)

-- For some reason, this definition (but no others) triggers
--
--   ERROR: brittany pretty printer returned syntactically invalid result.
--
-- brittany-disable-next-binding

instance EntityKeys (Entity a, Entity b, Entity c, Entity d) where
  type
    Keys (Entity a, Entity b, Entity c, Entity d) =
      (Key a, Key b, Key c, Key d)
  keys (a, b, c, d) = (entityKey a, entityKey b, entityKey c, entityKey d)
