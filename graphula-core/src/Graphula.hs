{-|
  Graphula is a compact interface for generating data and linking its
  dependencies. You can use this interface to generate fixtures for automated
  testing.

  The interface is extensible and supports pluggable front-ends.

  @
  runGraphIdentity . runGraphulaT $ do
    -- Compose dependencies at the value level
    Identity vet <- node @Veterinarian
    Identity owner <- nodeWith @Owner $ only vet
    -- TypeApplications is not necessary, but recommended for clarity.
    Identity dog <- nodeEditWith @Dog (owner, vet) $ \d ->
      d { name = "fido" }
  @
-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Graphula
  ( -- * Graph Declaration
    node
  , nodeEdit
  , nodeWith
  , nodeEditWith
  , GraphulaNode
  , GraphulaContext
  -- * Declaring Dependencies
  , HasDependencies(..)
  -- ** Singular Dependencies
  , Only(..)
  , only
  -- * The Graph Monad
  -- ** Type Classes
  , MonadGraphula
  , MonadGraphulaBackend(..)
  , MonadGraphulaFrontend(..)
  , EntityKeyGen(..)
  -- ** Backends
  , runGraphulaT
  , GraphulaT
  , runGraphulaLoggedT
  , runGraphulaLoggedWithFileT
  , GraphulaLoggedT
  , runGraphulaReplayT
  , GraphulaReplayT
  -- ** Frontends
  , runGraphulaIdempotentT
  , GraphulaIdempotentT
  -- * Extras
  , NoConstraint
  -- * Exceptions
  , GenerationFailure(..)
  ) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Aeson (FromJSON, Result(..), ToJSON, Value, eitherDecodeStrict', encode, fromJSON, toJSON)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (hPutStr)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq, ViewL(..), empty, viewl, (|>))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Database.Persist
  ( Entity
  , Key
  , PersistEntity
  , PersistEntityBackend
  , checkUnique
  , delete
  , entityKey
  , getEntity
  , insertKey
  , insertUnique
  )
import Database.Persist.Sql (SqlBackend)
import Generics.Eot (Eot, HasEot, fromEot, toEot)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import Graphula.Internal
import System.Directory (getTemporaryDirectory)
import System.IO (Handle, IOMode(..), hClose, openFile)
import System.IO.Temp (openTempFile)
import Test.HUnit.Lang (FailureReason(..), HUnitFailure(..), formatFailureReason)
import Test.QuickCheck (Arbitrary(..), Gen, generate)
import UnliftIO.Exception (Exception, SomeException, bracket, catch, mask, throwIO)

type MonadGraphula m =
  ( Monad m
  , MonadGraphulaBackend m
  , MonadGraphulaFrontend m
  , MonadIO m
  )

-- | A constraint over lists of nodes for 'MonadGraphula', and 'GraphulaNode'.
--
-- Helpful for defining utility functions over many nodes.
--
-- @
-- mkABC :: (GraphulaContext m '[A, B, C]) => m (Node m C)
-- mkABC = do
--   a <- node @A
--   b <- nodeWith @B (only a)
--   nodeEditWith @C (a, b) $ \n ->
--     n { cc = "spanish" }
-- @
--
type family GraphulaContext (m :: Type -> Type) (ts :: [Type]) :: Constraint where
   GraphulaContext m '[] = MonadGraphula m
   GraphulaContext m (t ': ts) = (GraphulaNode m t, GraphulaContext m ts)

class EntityKeyGen a where
  genEntityKey :: Gen (Maybe (Key a))
  genEntityKey = pure Nothing

class MonadGraphulaBackend m where
  type Logging m :: * -> Constraint
  -- ^ A constraint provided to log details of the graph to some form of
  --   persistence. This is used by 'runGraphulaLogged' to store graph nodes as
  --   JSON 'Value's.
  type Generate m :: * -> Constraint
  -- ^ A constraint for pluggable node generation. 'runGraphula'
  --   utilizes 'Arbitrary', 'runGraphulaReplay' utilizes 'FromJSON'.
  generateNode :: Generate m a => m a
  logNode :: Logging m a => a -> m ()

class MonadGraphulaFrontend m where
  insert
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, EntityKeyGen a)
    => Maybe (Key a) -> a -> m (Maybe (Entity a))
  remove :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Key a -> m ()


newtype RunDB backend n m = RunDB (forall b. ReaderT backend n b -> m b)

newtype GraphulaT n m a =
  GraphulaT { runGraphulaT' :: ReaderT (RunDB SqlBackend n m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RunDB SqlBackend n m))

instance MonadTrans (GraphulaT n) where
  lift = GraphulaT . lift

instance MonadUnliftIO m => MonadUnliftIO (GraphulaT n m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = GraphulaT $ withUnliftIO $ \u ->
    return $ UnliftIO $ unliftIO u . runGraphulaT'
  {-# INLINE withRunInIO #-}
  withRunInIO inner = GraphulaT $ withRunInIO $ \run ->
    inner $ run . runGraphulaT'

instance MonadIO m => MonadGraphulaBackend (GraphulaT n m) where
  type Logging (GraphulaT n m) = NoConstraint
  type Generate (GraphulaT n m) = Arbitrary
  generateNode = liftIO $ generate arbitrary
  logNode _ = pure ()

instance (MonadIO m, Applicative n, MonadIO n) => MonadGraphulaFrontend (GraphulaT n m) where
  insert mKey n = do
    RunDB runDB <- ask
    lift . runDB $ do
      case mKey of
        Nothing -> insertUnique n >>= \case
          Nothing -> pure Nothing
          Just key -> getEntity key
        Just key -> checkUnique n >>= \case
          Nothing -> do
            insertKey key n
            getEntity key
          Just _ -> pure Nothing
  remove key = do
    RunDB runDB <- ask
    lift . runDB $ delete key

runGraphulaT
  :: (MonadIO m)
  => (forall b . ReaderT SqlBackend n b -> m b)
  -> GraphulaT n m a
  -> m a
runGraphulaT runDB action = runGraphulaT' action `runReaderT` RunDB runDB


newtype GraphulaIdempotentT m a =
  GraphulaIdempotentT {runGraphulaIdempotentT' :: ReaderT (IORef (m ())) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef (m ())))

instance MonadUnliftIO m => MonadUnliftIO (GraphulaIdempotentT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = GraphulaIdempotentT $ withUnliftIO $ \u ->
    return $ UnliftIO $ unliftIO u . runGraphulaIdempotentT'
  {-# INLINE withRunInIO #-}
  withRunInIO inner = GraphulaIdempotentT $ withRunInIO $ \run ->
    inner $ run . runGraphulaIdempotentT'

instance MonadTrans GraphulaIdempotentT where
  lift = GraphulaIdempotentT . lift

instance (MonadIO m, MonadGraphulaFrontend m) => MonadGraphulaFrontend (GraphulaIdempotentT m) where
  insert mKey n = do
    finalizersRef <- ask
    mEnt <- lift $ insert mKey n
    for_ (entityKey <$> mEnt) $ \key ->
      liftIO $ modifyIORef' finalizersRef (remove key >>)
    pure mEnt
  remove = lift . remove

-- | A wrapper around a graphula frontend that produces finalizers to remove
-- graph nodes on error or completion. An idempotent graph produces no data
-- outside of its own closure.
--
-- @
-- runGraphIdentity . runGraphulaIdempotentT . runGraphulaT $ do
--   node @PancakeBreakfast
-- @
runGraphulaIdempotentT :: (MonadUnliftIO m) => GraphulaIdempotentT m a -> m a
runGraphulaIdempotentT action = mask $ \unmasked -> do
  finalizersRef <- liftIO . newIORef $ pure ()
  x <-
    unmasked
    $ runReaderT (runGraphulaIdempotentT' action) finalizersRef
    `catch` rollbackRethrow finalizersRef
  rollback finalizersRef $ pure x
 where
  rollback finalizersRef x = do
    finalizers <- liftIO $ readIORef finalizersRef
    finalizers >> x
  rollbackRethrow finalizersRef (e :: SomeException) =
    rollback finalizersRef (throwIO e)


newtype GraphulaLoggedT m a =
  GraphulaLoggedT {runGraphulaLoggedT' :: ReaderT (IORef (Seq Value)) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef (Seq Value)))

instance MonadTrans GraphulaLoggedT where
  lift = GraphulaLoggedT . lift

instance MonadIO m => MonadGraphulaBackend (GraphulaLoggedT m) where
  type Logging (GraphulaLoggedT m) = ToJSON
  type Generate (GraphulaLoggedT m) = Arbitrary
  generateNode = liftIO $ generate arbitrary
  logNode n = do
    graphLog <- ask
    liftIO $ modifyIORef' graphLog (|> toJSON n)

instance (Monad m, MonadGraphulaFrontend m) => MonadGraphulaFrontend (GraphulaLoggedT m) where
  insert mKey = lift . insert mKey
  remove = lift . remove

-- | An extension of 'runGraphulaT' that logs all json 'Value's to a temporary
-- file on 'Exception' and re-throws the 'Exception'.
runGraphulaLoggedT :: MonadUnliftIO m => GraphulaLoggedT m a -> m a
runGraphulaLoggedT = runGraphulaLoggedUsingT logFailTemp

-- | A variant of 'runGraphulaLoggedT' that accepts a file path to logged to
-- instead of utilizing a temp file.
runGraphulaLoggedWithFileT
  :: MonadUnliftIO m => FilePath -> GraphulaLoggedT m a -> m a
runGraphulaLoggedWithFileT logPath =
  runGraphulaLoggedUsingT $ logFailFile logPath

runGraphulaLoggedUsingT
  :: MonadUnliftIO m
  => (IORef (Seq Value) -> HUnitFailure -> m a)
  -> GraphulaLoggedT m a
  -> m a
runGraphulaLoggedUsingT logFail action = do
  graphLog <- liftIO $ newIORef empty
  runReaderT (runGraphulaLoggedT' action) graphLog `catch` logFail graphLog


newtype GraphulaReplayT m a =
  GraphulaReplayT {runGraphulaReplayT' :: ReaderT (IORef (Seq Value)) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef (Seq Value)))

instance MonadTrans GraphulaReplayT where
  lift = GraphulaReplayT . lift

instance MonadIO m => MonadGraphulaBackend (GraphulaReplayT m) where
  type Logging (GraphulaReplayT m) = NoConstraint
  type Generate (GraphulaReplayT m) = FromJSON
  generateNode = do
    replayRef <- ask
    mJsonNode <- popReplay replayRef
    case mJsonNode of
      Nothing -> throwIO $ userError "Not enough replay data to fullfill graph."
      Just jsonNode ->
        case fromJSON jsonNode of
          Error err -> throwIO $ userError err
          Success a -> pure a
  logNode _ = pure ()

instance (Monad m, MonadGraphulaFrontend m) => MonadGraphulaFrontend (GraphulaReplayT m) where
  insert mKey = lift . insert mKey
  remove = lift . remove

-- | Run a graph utilizing a JSON file for node generation via 'FromJSON'.
runGraphulaReplayT :: MonadUnliftIO m => FilePath -> GraphulaReplayT m a -> m a
runGraphulaReplayT replayFile action = do
  replayLog <- liftIO $ do
    bytes <- readFile replayFile
    case eitherDecodeStrict' bytes of
      Left err -> throwIO $ userError err
      Right nodes -> newIORef nodes
  runReaderT (runGraphulaReplayT' action) replayLog
    `catch` rethrowHUnitReplay replayFile

popReplay :: MonadIO m => IORef (Seq Value) -> m (Maybe Value)
popReplay ref = liftIO $ do
  nodes <- readIORef ref
  case viewl nodes of
    EmptyL -> pure Nothing
    n :< ns -> do
      writeIORef ref ns
      pure $ Just n


logFailUsing
  :: MonadIO m
  => IO (FilePath, Handle)
  -> IORef (Seq Value)
  -> HUnitFailure
  -> m a
logFailUsing f graphLog hunitfailure =
  flip rethrowHUnitLogged hunitfailure =<< logGraphToHandle graphLog f

logFailFile :: MonadIO m => FilePath -> IORef (Seq Value) -> HUnitFailure -> m a
logFailFile path = logFailUsing ((path, ) <$> openFile path WriteMode)

logFailTemp :: MonadIO m => IORef (Seq Value) -> HUnitFailure -> m a
logFailTemp =
  logFailUsing (flip openTempFile "fail-.graphula" =<< getTemporaryDirectory)

logGraphToHandle
  :: (MonadIO m) => IORef (Seq Value) -> IO (FilePath, Handle) -> m FilePath
logGraphToHandle graphLog openHandle = liftIO $ bracket
  openHandle
  (hClose . snd)
  (\(path, handle) ->
    readIORef graphLog >>= hPutStr handle . encode >> pure path
  )


rethrowHUnitWith :: MonadIO m => String -> HUnitFailure -> m a
rethrowHUnitWith message (HUnitFailure l r) =
  throwIO . HUnitFailure l . Reason $ message ++ "\n\n" ++ formatFailureReason r

rethrowHUnitLogged :: MonadIO m => FilePath -> HUnitFailure -> m a
rethrowHUnitLogged path =
  rethrowHUnitWith ("Graph dumped in temp file: " ++ path)

rethrowHUnitReplay :: MonadIO m => FilePath -> HUnitFailure -> m a
rethrowHUnitReplay filePath =
  rethrowHUnitWith ("Using graph file: " ++ filePath)


-- | Graphula accepts constraints for various uses. Frontends do not always
-- utilize these constraints. 'NoConstraint' is a universal class that all
-- types inhabit. It has no behavior and no additional constraints.
class NoConstraint a where

instance NoConstraint a where

class HasDependencies a where
  -- | A data type that contains values to be injected into @a@ via
  -- `dependsOn`. The default generic implementation of `dependsOn` supports
  -- tuples as 'Dependencies'. Data types with a single dependency should use
  -- 'Only' as a 1-tuple.
  --
  -- note: The contents of a tuple must be ordered as they appear in the
  -- definition of @a@.
  type Dependencies a
  type instance Dependencies a = ()

  -- | Assign values from the 'Dependencies' collection to a value.
  -- 'dependsOn' must be an idempotent operation.
  --
  -- Law:
  --
  -- prop> dependsOn . dependsOn = dependsOn
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
  deriving (Show, Typeable, Eq)

instance Exception GenerationFailure

type GraphulaNode m a =
  ( Generate m a
  , HasDependencies a
  , Logging m a
  , PersistEntityBackend a ~ SqlBackend
  , PersistEntity a
  , Typeable a
  , EntityKeyGen a
  )

{-|
  Generate and edit a value with data dependencies. This leverages
  'HasDependencies' to insert the specified data in the generated value. All
  dependency data is inserted after editing operations.

  > nodeEdit @Dog (ownerId, veterinarianId) $ \dog ->
  >   dog {name = "fido"}
-}
nodeEditWith
  :: forall a m
   . GraphulaContext m '[a]
  => Dependencies a
  -> (a -> a)
  -> m (Entity a)
nodeEditWith dependencies edits = 10 `attemptsToInsertWith` do
  x <- (`dependsOn` dependencies) . edits <$> generateNode
  logNode x
  pure x

{-|
  Generate a value with data dependencies. This leverages 'HasDependencies' to
  insert the specified data in the generated value.

  > nodeEdit @Dog (ownerId, veterinarianId)
-}
nodeWith
  :: forall a m . GraphulaContext m '[a] => Dependencies a -> m (Entity a)
nodeWith = flip nodeEditWith id

{-|
  Generate and edit a value that does not have any dependencies.

  > nodeEdit @Dog $ \dog -> dog {name = "fido"}
-}
nodeEdit
  :: forall a m
   . (GraphulaContext m '[a], Dependencies a ~ ())
  => (a -> a)
  -> m (Entity a)
nodeEdit = nodeEditWith ()

-- | Generate a value that does not have any dependencies
--
-- > node @Dog
node
  :: forall a m . (GraphulaContext m '[a], Dependencies a ~ ()) => m (Entity a)
node = nodeWith ()

attemptsToInsertWith
  :: forall a m
   . (Typeable a, GraphulaContext m '[a])
  => Int
  -> m a
  -> m (Entity a)
attemptsToInsertWith attempts source
  | 0 >= attempts = throwIO . GenerationFailureMaxAttempts $ typeRep
    (Proxy :: Proxy a)
  | otherwise = do
    value <- source
    mKey <- liftIO $ generate genEntityKey
    insert mKey value >>= \case
      Just a -> pure a
      Nothing -> pred attempts `attemptsToInsertWith` source

-- | For entities that only have singular 'Dependencies'. It uses data instead
-- of newtype to match laziness of builtin tuples.
data Only a = Only { fromOnly :: a }
  deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

only :: a -> Only a
only = Only
