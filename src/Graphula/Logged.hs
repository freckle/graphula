{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A version of 'GraphulaT' that logs the generated graph
module Graphula.Logged
  ( GraphulaLoggedT
  , runGraphulaLoggedT
  , runGraphulaLoggedWithFileT
  , runGraphulaLoggedUsingT
  ) where

import Prelude

import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence (Seq, empty, (|>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Graphula.Class
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.IO (Handle, IOMode (..), hClose, openFile)
import System.IO.Temp (openTempFile)
import Test.HUnit.Lang
  ( FailureReason (..)
  , HUnitFailure (..)
  , formatFailureReason
  )
import UnliftIO.Exception (bracket, catch, throwIO)

newtype GraphulaLoggedT m a = GraphulaLoggedT
  { runGraphulaLoggedT' :: ReaderT (IORef (Seq Text)) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (IORef (Seq Text))
    )

type role GraphulaLoggedT representational nominal

instance MonadUnliftIO m => MonadUnliftIO (GraphulaLoggedT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    GraphulaLoggedT $ withRunInIO $ \run -> inner $ run . runGraphulaLoggedT'

instance MonadTrans GraphulaLoggedT where
  lift = GraphulaLoggedT . lift

instance (MonadGraphulaBackend m, MonadIO m) => MonadGraphulaBackend (GraphulaLoggedT m) where
  type Logging (GraphulaLoggedT m) = Show
  askGen = lift askGen
  logNode n = do
    graphLog <- ask
    liftIO $ modifyIORef' graphLog (|> pack (show n))

instance (Monad m, MonadGraphulaFrontend m) => MonadGraphulaFrontend (GraphulaLoggedT m) where
  insert mKey = lift . insert mKey
  insertKeyed key = lift . insertKeyed key
  remove = lift . remove

-- | Run the graph while logging to a temporary file
runGraphulaLoggedT :: MonadUnliftIO m => GraphulaLoggedT m a -> m a
runGraphulaLoggedT = runGraphulaLoggedUsingT logFailTemp

-- | 'runGraphulaLoggedT', but to the specified file
runGraphulaLoggedWithFileT
  :: MonadUnliftIO m => FilePath -> GraphulaLoggedT m a -> m a
runGraphulaLoggedWithFileT = runGraphulaLoggedUsingT . logFailFile

-- | 'runGraphulaLoggedT', but using the custom action to accumulate
runGraphulaLoggedUsingT
  :: MonadUnliftIO m
  => (IORef (Seq Text) -> HUnitFailure -> m a)
  -> GraphulaLoggedT m a
  -> m a
runGraphulaLoggedUsingT logFail action = do
  graphLog <- liftIO $ newIORef empty
  runReaderT (runGraphulaLoggedT' action) graphLog `catch` logFail graphLog

logFailUsing
  :: MonadIO m
  => IO (FilePath, Handle)
  -> IORef (Seq Text)
  -> HUnitFailure
  -> m a
logFailUsing f graphLog hunitfailure =
  flip rethrowHUnitLogged hunitfailure =<< logGraphToHandle graphLog f

logFailFile :: MonadIO m => FilePath -> IORef (Seq Text) -> HUnitFailure -> m a
logFailFile path = logFailUsing ((path,) <$> openFile path WriteMode)

logFailTemp :: MonadIO m => IORef (Seq Text) -> HUnitFailure -> m a
logFailTemp = logFailUsing $ do
  tmp <- (++ "/graphula") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmp
  openTempFile tmp "fail-.graphula"

logGraphToHandle
  :: MonadIO m => IORef (Seq Text) -> IO (FilePath, Handle) -> m FilePath
logGraphToHandle graphLog openHandle =
  liftIO $
    bracket
      openHandle
      (hClose . snd)
      ( \(path, handle) -> do
          nodes <- readIORef graphLog
          path <$ traverse_ (T.hPutStrLn handle) nodes
      )

rethrowHUnitLogged :: MonadIO m => FilePath -> HUnitFailure -> m a
rethrowHUnitLogged path =
  rethrowHUnitWith ("Graph dumped in temp file: " ++ path)

rethrowHUnitWith :: MonadIO m => String -> HUnitFailure -> m a
rethrowHUnitWith message (HUnitFailure l r) =
  throwIO . HUnitFailure l . Reason $ message ++ "\n\n" ++ formatFailureReason r
