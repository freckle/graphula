{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Graphula.Free
  ( -- * Graph Declaration
    node
  , nodeEdit
  , nodeWith
  , nodeEditWith
  -- * Declaring Dependencies
  , HasDependencies(..)
  -- ** Singular Dependencies
  , Only(..)
  , only
  -- * The Graph Monad
  , Graph
  , runGraphula
  , runGraphulaLogged
  , runGraphulaLoggedWithFile
  , runGraphulaReplay
  , runGraphulaIdempotent
  , runGraphulaIdempotentLogged
  -- * Graph Implementation
  , Frontend(..)
  , MonadGraphulaFrontend(..)
  , Backend(..)
  , MonadGraphulaBackend(..)
  -- * Extras
  , NoConstraint
  -- * Exceptions
  , GenerationFailure(..)
  ) where

import Prelude hiding (readFile, lines)
import Test.QuickCheck (Arbitrary(..), generate)
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(..), formatFailureReason)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..), MonadMask(..), bracket)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT, iterT, liftF, transFreeT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (SomeException)
import Data.Aeson (ToJSON, FromJSON, Value, Result(..), toJSON, fromJSON, encode, eitherDecodeStrict')
import Data.ByteString (readFile)
import Data.ByteString.Lazy (hPutStr)
import Data.Foldable (for_)
import Data.Functor.Sum (Sum(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Sequence (Seq, ViewL(..), viewl, empty, (|>))
import GHC.Exts (Constraint)
import System.IO (hClose, openFile, IOMode(..), Handle)
import System.IO.Temp (openTempFile)
import System.Directory (getTemporaryDirectory)

import Graphula hiding (runGraphula)


-- | 'Graph' is a type alias for graphula's underlying monad. This type carries
-- constraints via 'ConstraintKinds', which enable pluggable frontends and backends.
--
-- * __generate__: A constraint for pluggable node generation. 'runGraphula'
--   utilizes 'Arbitrary', 'runGraphulaReplay' utilizes 'FromJSON'.
-- * __log__: A constraint provided to log details of the graph to some form of
--   persistence. This is used by 'runGraphulaLogged' to store graph nodes as
--   JSON 'Value's.
-- * __nodeConstraint__: A constraint applied to nodes. This is utilized during
--   insertion and can be leveraged by frontends with typeclass interfaces
--   to insertion.
-- * __entity__: A wrapper type used to return relevant information about a given
--   node. `graphula-persistent` returns all nodes in the 'Entity' type.
type Graph generate log nodeConstraint entity
  = FreeT (Sum (Backend generate log) (Frontend nodeConstraint entity))

instance Monad m => MonadGraphulaFrontend (Graph generate log nodeConstraint entity m) where
  type NodeConstraint (Graph generate log nodeConstraint entity m) = nodeConstraint
  type Node (Graph generate log nodeConstraint entity m) = entity
  insert = liftRight . insert
  remove = liftRight . remove

instance Monad m => MonadGraphulaFrontend (FreeT (Frontend nodeConstraint entity) m) where
  type NodeConstraint (FreeT (Frontend nodeConstraint entity) m) = nodeConstraint
  type Node (FreeT (Frontend nodeConstraint entity) m) = entity
  insert n = liftF (Insert n id)
  remove n = liftF (Remove n ())

instance Monad m => MonadGraphulaBackend (Graph generate log nodeConstraint entity m) where
  type Logging (Graph generate log nodeConstraint entity m) = log
  type Generate (Graph generate log nodeConstraint entity m) = generate
  generateNode = liftLeft . liftF $ GenerateNode id
  logNode a = liftLeft . liftF $ LogNode a (const ())


liftLeft :: (Monad m, Functor f, Functor g) => FreeT f m a -> FreeT (Sum f g) m a
liftLeft = transFreeT InL

liftRight :: (Monad m, Functor f, Functor g) => FreeT g m a -> FreeT (Sum f g) m a
liftRight = transFreeT InR


data Frontend (nodeConstraint :: * -> Constraint) entity next where
  Insert :: nodeConstraint a => a -> (Maybe (entity a) -> next) -> Frontend nodeConstraint entity next
  Remove :: nodeConstraint a => entity a -> next -> Frontend nodeConstraint entity next

deriving instance Functor (Frontend nodeConstraint entity)

data Backend (generate :: * -> Constraint) (log :: * -> Constraint) next where
  GenerateNode :: (generate a) => (a -> next) -> Backend generate log next
  LogNode :: (log a) => a -> (() -> next) -> Backend generate log next

deriving instance Functor (Backend generate log)

runGraphulaUsing
  :: (MonadIO m, MonadCatch m)
  => (Backend generate log (m a) -> m a)
  -> (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph generate log nodeConstraint entity m a
  -> m a
runGraphulaUsing backend frontend f =
  flip iterT f $ \case
    InR r -> frontend r
    InL l -> backend l

-- | Interpret a 'Graph' with a given 'Frontend' interpreter, utilizing
-- 'Arbitrary' for node generation.
runGraphula
  :: (MonadIO m, MonadCatch m)
  => (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary NoConstraint nodeConstraint entity m a
  -> m a
runGraphula = runGraphulaUsing backendArbitrary

-- | An extension of 'runGraphula' that produces finalizers to remove graph nodes
-- on error or completion. An idempotent 'Graph' produces no data outside of its
-- own closure.
runGraphulaIdempotent
  :: (MonadIO m, MonadCatch m, MonadMask m)
  => (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary NoConstraint nodeConstraint entity m a -> m a
runGraphulaIdempotent = runGraphulaIdempotentUsing backendArbitrary

-- | An extension of 'runGraphulaIdemptotent' that produces replayable logs, like
-- 'runGraphulaLogged'.
runGraphulaIdempotentLogged
  :: (MonadIO m, MonadCatch m, MonadMask m)
  => (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary ToJSON nodeConstraint entity m a -> m a
runGraphulaIdempotentLogged frontend graph = do
  graphLog <- liftIO $ newIORef empty
  go graphLog `catch` logFailTemp graphLog
  where
    go graphLog = runGraphulaIdempotentUsing (backendArbitraryLogged graphLog) frontend graph

runGraphulaIdempotentUsing
  :: (MonadIO m, MonadCatch m, MonadMask m)
  => (Backend generate log (m a) -> m a)
  -> (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph generate log nodeConstraint entity m a -> m a
runGraphulaIdempotentUsing backend frontend f =
  mask $ \unmasked -> do
    finalizersRef <- liftIO . newIORef $ pure ()
    x <- unmasked $ interpret finalizersRef `catch` rollbackRethrow finalizersRef
    rollback finalizersRef $ pure x
  where
    interpret finalizersRef =
      runGraphulaUsing backend (iterT frontend . finalizerFrontend finalizersRef) f
    rollback finalizersRef x = do
      finalizers <- liftIO $ readIORef finalizersRef
      iterT frontend (finalizers >> x)
    rollbackRethrow finalizersRef (e :: SomeException) =
      rollback finalizersRef (throwM e)

-- | An extension of 'runGraphula' that logs all json 'Value's to a temporary
-- file on 'Exception' and re-throws the 'Exception'.
runGraphulaLogged
  :: (MonadIO m, MonadCatch m)
  => (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary ToJSON nodeConstraint entity m a
  -> m a
runGraphulaLogged =
  runGraphulaLoggedUsing logFailTemp

-- | A variant of 'runGraphulaLogged' that accepts a file path to logged to
-- instead of utilizing a temp file.
runGraphulaLoggedWithFile
  :: (MonadIO m, MonadCatch m)
  => FilePath
  -> (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary ToJSON nodeConstraint entity m a
  -> m a
runGraphulaLoggedWithFile logPath =
  runGraphulaLoggedUsing (logFailFile logPath)

runGraphulaLoggedUsing
  :: (MonadIO m, MonadCatch m)
  => (IORef (Seq Value) -> HUnitFailure -> m a)
  -> (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph Arbitrary ToJSON nodeConstraint entity m a
  -> m a
runGraphulaLoggedUsing logFail frontend f = do
  graphLog <- liftIO $ newIORef empty
  runGraphulaUsing (backendArbitraryLogged graphLog) frontend f
    `catch` logFail graphLog

-- | Interpret a 'Graph' with a given 'Frontend' interpreter, utilizing a JSON
-- file for node generation via 'FromJSON'.
runGraphulaReplay
  :: (MonadIO m, MonadCatch m)
  => FilePath
  -> (Frontend nodeConstraint entity (m a) -> m a)
  -> Graph FromJSON NoConstraint nodeConstraint entity m a -> m a
runGraphulaReplay replayFile frontend f = do
  replayLog <-
    liftIO $ do
      bytes <- readFile replayFile
      case eitherDecodeStrict' bytes of
        Left err -> throwM $ userError err
        Right nodes -> newIORef nodes
  runGraphulaUsing (backendReplay replayLog) frontend f
    `catch` rethrowHUnitReplay replayFile


finalizerFrontend
  :: (MonadThrow m, MonadIO m)
  => IORef (FreeT (Frontend nodeConstraint entity) m ())
  -> Frontend nodeConstraint entity (m a)
  -> FreeT (Frontend nodeConstraint entity) m a
finalizerFrontend finalizersRef f = case f of
  Insert n next -> do
    mEnt <- liftF $ Insert n id
    for_ mEnt $ \ent ->
      liftIO $ modifyIORef' finalizersRef (remove ent >>)
    lift $ next mEnt
  Remove ent next -> do
    remove ent
    lift next

backendArbitrary :: (MonadThrow m, MonadIO m) => Backend Arbitrary NoConstraint (m b) -> m b
backendArbitrary = \case
  GenerateNode next -> do
    a <- liftIO . generate $ arbitrary
    next a
  LogNode _ next -> next ()

backendArbitraryLogged :: (MonadThrow m, MonadIO m) => IORef (Seq Value) -> Backend Arbitrary ToJSON (m b) -> m b
backendArbitraryLogged graphLog = \case
  GenerateNode next -> do
    a <- liftIO . generate $ arbitrary
    next a
  LogNode a next -> do
    liftIO $ modifyIORef' graphLog (|> toJSON a)
    next ()

backendReplay :: (MonadThrow m, MonadIO m) => IORef (Seq Value) -> Backend FromJSON NoConstraint (m b) -> m b
backendReplay replayRef = \case
  GenerateNode next -> do
    mJsonNode <- popReplay replayRef
    case mJsonNode of
      Nothing -> throwM $ userError "Not enough replay data to fullfill graph."
      Just jsonNode ->
        case fromJSON jsonNode of
          Error err -> throwM $ userError err
          Success a -> next a
  LogNode _ next -> next ()

popReplay :: MonadIO m => IORef (Seq Value) -> m (Maybe Value)
popReplay ref = liftIO $ do
  nodes <- readIORef ref
  case viewl nodes of
    EmptyL -> pure Nothing
    n :< ns -> do
      writeIORef ref ns
      pure $ Just n

logFailUsing :: (MonadIO m, MonadThrow m) => IO (FilePath, Handle) -> IORef (Seq Value) -> HUnitFailure -> m a
logFailUsing f graphLog hunitfailure =
  flip rethrowHUnitLogged hunitfailure =<< logGraphToHandle graphLog f

logFailFile :: (MonadIO m, MonadThrow m) => FilePath -> IORef (Seq Value) -> HUnitFailure -> m a
logFailFile path =
  logFailUsing ((path, ) <$> openFile path WriteMode)

logFailTemp :: (MonadIO m, MonadThrow m) => IORef (Seq Value) -> HUnitFailure -> m a
logFailTemp =
  logFailUsing (flip openTempFile "fail-.graphula" =<< getTemporaryDirectory)

logGraphToHandle :: (MonadIO m) => IORef (Seq Value) -> IO (FilePath, Handle) -> m FilePath
logGraphToHandle graphLog openHandle =
  liftIO $ bracket
    openHandle
    (hClose . snd)
    (\(path, handle) -> readIORef graphLog >>= hPutStr handle . encode >> pure path )


rethrowHUnitWith :: MonadThrow m => String -> HUnitFailure -> m a
rethrowHUnitWith message (HUnitFailure l r)  =
  throwM . HUnitFailure l . Reason $ message ++ "\n\n" ++ formatFailureReason r

rethrowHUnitLogged :: MonadThrow m => FilePath -> HUnitFailure -> m a
rethrowHUnitLogged path  =
  rethrowHUnitWith ("Graph dumped in temp file: " ++ path)

rethrowHUnitReplay :: (MonadIO m, MonadThrow m) => FilePath -> HUnitFailure -> m a
rethrowHUnitReplay filePath =
  rethrowHUnitWith ("Using graph file: " ++ filePath)
