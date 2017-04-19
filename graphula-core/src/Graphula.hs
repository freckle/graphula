{-# LANGUAGE ConstraintKinds #-}
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

module Graphula
  ( node
  , nodeEdit
  , nodeWith
  , nodeEditWith
  , HasDependencies(..)
  , Graph
  , runGraphula
  , runGraphulaLogged
  , runGraphulaReplay
  , Frontend(..)
  , Backend(..)
  , NoConstraint
  , Only(..)
  , only
  ) where

import Prelude hiding (readFile, lines)
import Test.QuickCheck (Arbitrary(..), generate)
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(..), formatFailureReason)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Trans.Free (FreeT, iterT, liftF, transFreeT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (Exception, bracket)
import Data.Semigroup ((<>))
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Data.ByteString (ByteString, hPutStr, readFile)
import Data.ByteString.Char8 (lines)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Functor.Sum (Sum(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, TypeRep, typeRep)
import Generics.Eot (fromEot, toEot, Eot, HasEot)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import System.Directory (getTemporaryDirectory)

import Graphula.Internal

type Graph generate log nodeConstraint entity
  = FreeT (Sum (Backend generate log) (Frontend nodeConstraint entity))

runGraphula
  :: (MonadIO m, MonadCatch m)
  => (Frontend nodeConstraint entity (m a) -> m a) -> Graph Arbitrary NoConstraint nodeConstraint entity m a -> m a
runGraphula frontend f =
  flip iterT f $ \case
    InR r -> frontend r
    InL l -> backendArbitrary l

runGraphulaLogged
  :: (MonadIO m, MonadCatch m)
  => (Frontend nodeConstraint entity (m a) -> m a) -> Graph Arbitrary ToJSON nodeConstraint entity m a -> m a
runGraphulaLogged frontend f = do
  graphLog <- liftIO $ newIORef ""
  catch (go graphLog) (logFail graphLog)
  where
    go graphLog =
      flip iterT f $ \case
        InR r -> frontend r
        InL l -> backendArbitraryLogged graphLog l

runGraphulaReplay
  :: (MonadIO m, MonadCatch m)
  => (Frontend nodeConstraint entity (m a) -> m a) -> FilePath -> Graph FromJSON NoConstraint nodeConstraint entity m a -> m a
runGraphulaReplay frontend replayFile f = do
  replayLog <- liftIO $ newIORef =<< (lines <$> readFile replayFile)
  catch (go replayLog) (replayFail replayFile)
  where
    go replayLog =
      flip iterT f $ \case
        InR r -> frontend r
        InL l -> backendReplay replayLog l



backendArbitrary :: (MonadThrow m, MonadIO m) => Backend Arbitrary NoConstraint (m b) -> m b
backendArbitrary = \case
  GenerateNode next -> do
    a <- liftIO . generate $ arbitrary
    next a
  LogNode a next -> next ()
  Throw e next ->
    next =<< throwM e

backendArbitraryLogged :: (MonadThrow m, MonadIO m) => IORef ByteString -> Backend Arbitrary ToJSON (m b) -> m b
backendArbitraryLogged graphLog = \case
  GenerateNode next -> do
    a <- liftIO . generate $ arbitrary
    next a
  LogNode a next -> do
    liftIO $ modifyIORef' graphLog (<> (toStrict (encode a) <> "\n"))
    next ()
  Throw e next ->
    next =<< throwM e

backendReplay :: (MonadThrow m, MonadIO m) => IORef [ByteString] -> Backend FromJSON NoConstraint (m b) -> m b
backendReplay replayRef = \case
  GenerateNode next -> do
    mJsonNode <- popReplay replayRef
    case mJsonNode of
      Nothing -> throwM $ userError "Not enough replay data to fullfill graph."
      Just jsonNode ->
        case eitherDecode $ fromStrict jsonNode of
          Left err -> throwM $ userError err
          Right a -> next a
  LogNode _ next -> next ()
  Throw e next ->
    next =<< throwM e

popReplay :: MonadIO m => IORef [ByteString] -> m (Maybe ByteString)
popReplay ref = liftIO $ do
  nodes <- readIORef ref
  case nodes of
    [] -> pure Nothing
    head:rest -> do
      writeIORef ref rest
      pure $ Just head

logFail :: (MonadIO m, MonadThrow m) => IORef ByteString -> HUnitFailure -> m a
logFail graphLog (HUnitFailure l r) = do
  path <- graphToTempFile graphLog
  throwM $ HUnitFailure l $ Reason
     $ "Graph dumped in temp file: " ++ path  ++ "\n\n"
    ++ formatFailureReason r

graphToTempFile :: (MonadIO m) => IORef ByteString -> m FilePath
graphToTempFile graphLog =
  liftIO $ bracket
    (flip openTempFile "fail-.graphula" =<< getTemporaryDirectory)
    (hClose . snd)
    (\(path, handle) -> readIORef graphLog >>= hPutStr handle >> pure path )

replayFail :: (MonadIO m, MonadThrow m) => FilePath -> HUnitFailure -> m a
replayFail filePath (HUnitFailure l r) =
  throwM $ HUnitFailure l $ Reason
     $ "Using graph file: " ++ filePath  ++ "\n\n"
    ++ formatFailureReason r

liftLeft :: (Monad m, Functor f, Functor g) => FreeT f m a -> FreeT (Sum f g) m a
liftLeft = transFreeT InL

liftRight :: (Monad m, Functor f, Functor g) => FreeT g m a -> FreeT (Sum f g) m a
liftRight = transFreeT InR


data Frontend (nodeConstraint :: * -> Constraint) entity next where
  Insert :: nodeConstraint a => a -> (Maybe (entity a) -> next) -> Frontend nodeConstraint entity next

deriving instance Functor (Frontend nodeConstraint entity)

insert :: (Monad m, nodeConstraint a) => a -> Graph generate log nodeConstraint entity m (Maybe (entity a))
insert n = liftRight $ liftF (Insert n id)


data Backend (generate :: * -> Constraint) (log :: * -> Constraint) next where
  GenerateNode :: (generate a) => (a -> next) -> Backend generate log next
  LogNode :: (log a) => a -> (() -> next) -> Backend generate log next
  Throw :: Exception e => e -> (a -> next) -> Backend generate log next

deriving instance Functor (Backend generate log)

generateNode :: (Monad m, generate a) => Graph generate log nodeConstraint entity m a
generateNode = liftLeft . liftF $ GenerateNode id

logNode :: (Monad m, log a) => a -> Graph generate log nodeConstraint entity m ()
logNode a = liftLeft . liftF $ LogNode a (const ())

throw :: (Monad m, Exception e) => e -> Graph generate log nodeConstraint entity m a
throw e = liftLeft . liftF $ Throw e id

class NoConstraint a where

instance NoConstraint a where

class HasDependencies a where
  type Dependencies a
  type instance Dependencies a = ()

  dependsOn :: a -> Dependencies a -> a
  default dependsOn
    ::
      ( HasEot a
      , HasEot (Dependencies a)
      , GHasDependencies (Proxy a) (Proxy (Dependencies a)) (Eot a) (Eot (Dependencies a))
      )
    => a -> Dependencies a -> a
  dependsOn a dependencies =
    fromEot $
      genericDependsOn
        (Proxy :: Proxy a)
        (Proxy :: Proxy (Dependencies a))
        (toEot a)
        (toEot dependencies)

data GenerationFailure =
  GenerationFailureMaxAttempts TypeRep
  deriving (Show, Typeable)

instance Exception GenerationFailure

nodeEditWith
  :: forall a generate log entity nodeConstraint m. (Monad m, nodeConstraint a, Typeable a, generate a, log a, HasDependencies a)
  => Dependencies a -> (a -> a) -> Graph generate log nodeConstraint entity m (entity a)
nodeEditWith dependencies edits =
  tryInsert 10 0 $ do
    x <- generateNode
    logNode x
    pure (edits x `dependsOn` dependencies)

nodeWith
  :: forall a generate log entity nodeConstraint m. (Monad m, nodeConstraint a, Typeable a, generate a, log a, HasDependencies a)
  => Dependencies a -> Graph generate log nodeConstraint entity m (entity a)
nodeWith = flip nodeEditWith id

nodeEdit
  :: forall a generate log entity nodeConstraint m. (Monad m, nodeConstraint a, Typeable a, generate a, log a, HasDependencies a, Dependencies a ~ ())
  => (a -> a) -> Graph generate log nodeConstraint entity m (entity a)
nodeEdit = nodeEditWith ()

node
  :: forall a generate log entity nodeConstraint m. (Monad m, nodeConstraint a, Typeable a, generate a, log a, HasDependencies a, Dependencies a ~ ())
  => Graph generate log nodeConstraint entity m (entity a)
node = nodeWith ()

tryInsert
  :: forall a generate log entity nodeConstraint m. (Monad m, nodeConstraint a, Typeable a)
  => Int -> Int -> Graph generate log nodeConstraint entity m a -> Graph generate log nodeConstraint entity m (entity a)
tryInsert maxAttempts currentAttempts source
  | currentAttempts >= maxAttempts =
      throw . GenerationFailureMaxAttempts $ typeRep (Proxy :: Proxy a)
  | otherwise = do
    value <- source
    insert value >>= \case
      Just a -> pure a
      Nothing -> tryInsert maxAttempts (succ currentAttempts) source

-- For entities that only have one dependency
newtype Only a = Only { fromOnly :: a }
  deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

only :: a -> Only a
only = Only
