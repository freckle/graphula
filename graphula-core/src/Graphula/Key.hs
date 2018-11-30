{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphula.Key (onlyKey, keys, Keys) where

import Database.Persist
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Graphula (Only(..), only)

class EntityKeys a where
  type Keys a
  keys :: a -> Keys a

instance
  ( TypeError
    ( 'Text "Cannot use naked ‘" ':<>: 'ShowType (Entity a) ':<>:
      'Text "’ as argument to ‘keys’." ':$$:
      'Text "Did you mean ‘Only (" ':<>:
      'ShowType (Entity a) ':<>: 'Text ")’?"
    )
  ) => EntityKeys (Entity a) where
  type Keys (Entity a) = Key a
  keys = entityKey

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

instance EntityKeys (Entity a, Entity b, Entity c, Entity d) where
  type Keys (Entity a, Entity b, Entity c, Entity d) = (Key a, Key b, Key c, Key d)
  keys (a, b, c, d) = (entityKey a, entityKey b, entityKey c, entityKey d)
