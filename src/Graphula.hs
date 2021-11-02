{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
--
-- Graphula is a compact interface for generating data and linking its
-- dependencies. You can use this interface to generate fixtures for automated
-- testing.
--
-- @
-- {- config/models
--
-- School
--   name Text
--   deriving Generic
--
-- Teacher
--   schoolId SchoolId
--   name Text
--   deriving Generic
--
-- Course
--   schoolId SchoolId
--   teacherId TeacherId
--   name Text
--   deriving Generic
--
-- -}
--
-- instance Arbitrary School where
--   -- ...
--
-- instance Arbitrary Teacher where
--   -- ...
--
-- instance Arbitrary Course where
--   -- ...
--
-- instance 'HasDependencies' School
--
-- instance 'HasDependencies' Teacher where
--   type Dependencies Teacher = Only SchoolId
--
-- instance 'HasDependencies' Course where
--   type Dependencies Course = (SchoolId, CourseId)
--
-- 'runGraphulaT' runDB $ do
--   school <- 'node' \@School () mempty
--
--   teacher <- 'node' \@Teacher ('onlyKey' school)
--      $ edit
--      $ \t -> t { teacherName = \"Alice\" }
--
--   course <- 'node' \@Course ('keys' (school, teacher))
--      $ 'ensure'
--      $ not . courseIsArchived
-- @
--
module Graphula
  (
  -- * Basic usage
  -- ** Model requirements
    HasDependencies(..)
  , Only(..)
  , only

  -- ** Defining the graph
  , node
  , edit
  , ensure

  -- ** Running the graph
  , GraphulaT
  , runGraphulaT
  , GenerationFailure(..)

  -- * Advanced usage
  -- ** Non-serial keys
  , KeySourceType(..)
  , nodeKeyed

  -- ** Running with logging
  , GraphulaLoggedT
  , runGraphulaLoggedT
  , runGraphulaLoggedWithFileT

  -- ** Running idempotently
  , GraphulaIdempotentT
  , runGraphulaIdempotentT

  -- * Useful synonymns
  -- |
  --
  -- When declaring your own functions that call 'node', these synonyms can help
  -- with the constraint soup.
  --
  -- > genSchoolWithTeacher
  -- >   :: GraphulaContext m '[School, Teacher]
  -- >   -> m (Entity Teacher)
  -- > genSchoolWithTeacher = do
  -- >   school <- node @School () mempty
  -- >   node @Teacher (onlyKey school) mempty
  --
  , GraphulaContext
  , GraphulaNode

  -- * Lower-level details
  -- |
  --
  -- These exports are likely to be removed from this module in a future
  -- version. If you are using them, consider importing from their own modules.
  --
  , MonadGraphula
  , MonadGraphulaBackend(..)
  , MonadGraphulaFrontend(..)
  , NodeOptions
  , GenerateKey
  , NoConstraint
  ) where

import Prelude hiding (readFile)

import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.IORef (IORef, newIORef)
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import Database.Persist
  ( PersistEntity
  , PersistEntityBackend
  , checkUnique
  , delete
  , get
  , getEntity
  , insertKey
  , insertUnique
  )
import Database.Persist.Sql (SqlBackend)
import Graphula.Class
import Graphula.Dependencies
import Graphula.Idempotent
import Graphula.Logged
import Graphula.NoConstraint
import Graphula.Node
import System.Random (randomIO)
import Test.HUnit.Lang
  (FailureReason(..), HUnitFailure(..), formatFailureReason)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Random (QCGen, mkQCGen)
import UnliftIO.Exception (catch, throwIO)

-- | A constraint over lists of nodes for 'MonadGraphula', and 'GraphulaNode'.
--
-- Helpful for defining utility functions over many nodes.
--
-- @
-- mkABC :: (GraphulaContext m '[A, B, C]) => m (Node m C)
-- mkABC = do
--   a <- node @A () mempty
--   b <- node @B (only a) mempty
--   node @C (a, b) $ edit $ \n ->
--     n { cc = "spanish" }
-- @
--
type family GraphulaContext (m :: Type -> Type) (ts :: [Type]) :: Constraint where
   GraphulaContext m '[] = MonadGraphula m
   GraphulaContext m (t ': ts) = (GraphulaNode m t, GraphulaContext m ts)

data Args backend n m = Args
  { dbRunner :: RunDB backend n m
  , gen :: IORef QCGen
  }

newtype RunDB backend n m = RunDB (forall b. ReaderT backend n b -> m b)

newtype GraphulaT n m a =
  GraphulaT { runGraphulaT' :: ReaderT (Args SqlBackend n m) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Args SqlBackend n m))

instance MonadTrans (GraphulaT n) where
  lift = GraphulaT . lift

instance MonadUnliftIO m => MonadUnliftIO (GraphulaT n m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    GraphulaT $ withRunInIO $ \run -> inner $ run . runGraphulaT'

instance MonadIO m => MonadGraphulaBackend (GraphulaT n m) where
  type Logging (GraphulaT n m) = NoConstraint
  askGen = asks gen
  logNode _ = pure ()

instance (MonadIO m, Applicative n, MonadIO n) => MonadGraphulaFrontend (GraphulaT n m) where
  insert mKey n = do
    RunDB runDB <- asks dbRunner
    lift . runDB $ case mKey of
      Nothing -> insertUnique n >>= \case
        Nothing -> pure Nothing
        Just key -> getEntity key
      Just key -> do
        existingKey <- get key
        whenNothing existingKey $ do
          existingUnique <- checkUnique n
          whenNothing existingUnique $ do
            insertKey key n
            getEntity key

  remove key = do
    RunDB runDB <- asks dbRunner
    lift . runDB $ delete key

whenNothing :: Applicative m => Maybe a -> m (Maybe b) -> m (Maybe b)
whenNothing Nothing f = f
whenNothing (Just _) _ = pure Nothing

runGraphulaT
  :: (MonadUnliftIO m)
  => Maybe Int -- ^ Optional seed
  -> (forall b . ReaderT SqlBackend n b -> m b) -- ^ Database runner
  -> GraphulaT n m a
  -> m a
runGraphulaT mSeed runDB action = do
  seed <- maybe (liftIO randomIO) pure mSeed
  qcGen <- liftIO $ newIORef $ mkQCGen seed
  runReaderT (runGraphulaT' action) (Args (RunDB runDB) qcGen)
    `catch` logFailingSeed seed

logFailingSeed :: MonadIO m => Int -> HUnitFailure -> m a
logFailingSeed seed = rethrowHUnitWith ("Graphula with seed: " ++ show seed)

rethrowHUnitWith :: MonadIO m => String -> HUnitFailure -> m a
rethrowHUnitWith message (HUnitFailure l r) =
  throwIO . HUnitFailure l . Reason $ message ++ "\n\n" ++ formatFailureReason r

type GraphulaNode m a
  = ( HasDependencies a
    , Logging m a
    , PersistEntityBackend a ~ SqlBackend
    , PersistEntity a
    , Typeable a
    , Arbitrary a
    )
