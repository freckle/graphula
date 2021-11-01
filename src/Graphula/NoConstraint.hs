{-# LANGUAGE FlexibleInstances #-}

-- | An empty 'Constraint'
--
-- Graphula accepts constraints for various uses. Frontends do not always
-- utilize these constraints. 'NoConstraint' is a universal class that all types
-- inhabit. It has no behavior and no additional constraints.
--
module Graphula.NoConstraint
  ( NoConstraint
  ) where

class NoConstraint a
instance NoConstraint a
