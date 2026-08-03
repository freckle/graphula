{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

module Graphula.ExceptionContext
  ( GraphulaExceptionContext (..)
  , addExceptionContext
  ) where

import Prelude

#if MIN_VERSION_base(4,20,0)
import Control.Exception (addExceptionContext)
import Control.Exception.Annotation (ExceptionAnnotation)
#else
import Control.Exception (SomeException)
#endif

newtype GraphulaExceptionContext = GraphulaExceptionContext
  { graphulaExceptionContextSeed :: Int
  }
  deriving stock (Show)

#if MIN_VERSION_base(4,20,0)
instance ExceptionAnnotation GraphulaExceptionContext
#else
addExceptionContext
  :: a
  -- ^ Argument ignored due to @base < 4.20@
  -> SomeException
  -> SomeException
addExceptionContext _ = id
#endif
