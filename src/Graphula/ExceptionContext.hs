{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

module Graphula.ExceptionContext
  ( GraphulaExceptionContext (..)
  , throwWithGraphulaExceptionContext
  ) where

import Prelude

import Control.Exception (SomeException (..), throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

#if MIN_VERSION_base(4,20,0)
import Control.Exception (ExceptionWithContext (..), someExceptionContext)
import Control.Exception.Annotation (ExceptionAnnotation)
import Control.Exception.Context (addExceptionAnnotation)
#endif

newtype GraphulaExceptionContext = GraphulaExceptionContext
  { graphulaExceptionContextSeed :: Int
  }
  deriving stock (Show)

#if MIN_VERSION_base(4,20,0)
instance ExceptionAnnotation GraphulaExceptionContext
#endif

-- | Attach the seed as exception context, then rethrow
--
-- On @base < 4.20@, where exception context does not exist, this simply
-- rethrows the given exception unchanged.
throwWithGraphulaExceptionContext
  :: MonadIO m
  => GraphulaExceptionContext
  -> SomeException
  -> m a
#if MIN_VERSION_base(4,20,0)
throwWithGraphulaExceptionContext ctx ex@(SomeException e) =
  liftIO
    . throwIO
    $ ExceptionWithContext
      (addExceptionAnnotation ctx (someExceptionContext ex))
      e
#else
throwWithGraphulaExceptionContext _ctx ex = liftIO $ throwIO ex
#endif
