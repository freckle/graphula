{-|
  Graphula is a compact interface for generating data and linking its
  dependencies. You can use this interface to generate fixtures for automated
  testing.

  The interface is extensible and supports pluggable front-ends.

  @
  runGraphIdentity . runGraphulaT $ do
    -- Compose dependencies at the value level
    Identity vet <- node @Veterinarian () mempty
    Identity owner <- node @Owner (only vet) mempty
    -- TypeApplications is not necessary, but recommended for clarity.
    Identity dog <- node @Dog (owner, vet) $ edit $ \d -> d { name = "fido" }
  @
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Graphula
  ( -- * Graph Declaration
    node
  , GraphulaNode
  , GraphulaContext
    -- ** Node options
  , NodeOptions
  , edit
  , suchThat
    -- * Declaring Dependencies and key source
  , HasDependencies(..)
  , KeySourceType(..)
  , KeyStrategy(..)
    -- ** Singular Dependencies
  , Only(..)
  , only
    -- * The Graph Monad
    -- ** Type Classes
  , MonadGraphula
  , MonadGraphulaBackend(..)
  , MonadGraphulaFrontend(..)
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
  )
where

import Prelude hiding (readFile)

import Control.Monad (guard, (<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Aeson
  ( FromJSON
  , Result(..)
  , ToJSON
  , Value
  , eitherDecodeStrict'
  , encode
  , fromJSON
  , toJSON
  )
import Data.ByteString (readFile)
import Data.ByteString.Lazy (hPutStr)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Semigroup.Generic (gmappend, gmempty)
import Data.Sequence (Seq, ViewL(..), empty, viewl, (|>))
import Data.Traversable (for)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Database.Persist
  ( Entity(..)
  , Key
  , PersistEntity
  , PersistEntityBackend
  , checkUnique
  , delete
  , entityKey
  , get
  , getEntity
  , insertKey
  , insertUnique
  )
import Database.Persist.Sql (SqlBackend)
import Generics.Eot (Eot, HasEot, fromEot, toEot)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import Graphula.Internal
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.IO (Handle, IOMode(..), hClose, openFile)
import System.IO.Temp (openTempFile)
import Test.HUnit.Lang
  (FailureReason(..), HUnitFailure(..), formatFailureReason)
import Test.QuickCheck (Arbitrary(..), Gen, generate)
import UnliftIO.Exception
  (Exception, SomeException, bracket, catch, mask, throwIO)

type MonadGraphula m
  = (Monad m, MonadGraphulaBackend m, MonadGraphulaFrontend m, MonadIO m)

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
    :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m)
    => Maybe (Key a) -> a -> m (Maybe (Entity a))
  remove :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a, Monad m) => Key a -> m ()


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
  withRunInIO inner =
    GraphulaT $ withRunInIO $ \run -> inner $ run . runGraphulaT'

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
        Nothing -> do
          insertUnique n >>= \case
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
    RunDB runDB <- ask
    lift . runDB $ delete key

whenNothing :: Applicative m => Maybe a -> m (Maybe b) -> m (Maybe b)
whenNothing Nothing f = f
whenNothing (Just _) _ = pure Nothing

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
    for_ (entityKey <$> mEnt)
      $ \key -> liftIO $ modifyIORef' finalizersRef (remove key >>)
    pure mEnt
  remove = lift . remove

-- | A wrapper around a graphula frontend that produces finalizers to remove
-- graph nodes on error or completion. An idempotent graph produces no data
-- outside of its own closure.
--
-- @
-- runGraphIdentity . runGraphulaIdempotentT . runGraphulaT $ do
--   node @PancakeBreakfast () mempty
-- @
--
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
      Nothing ->
        throwIO $ userError "Not enough replay data to fullfill graph."
      Just jsonNode -> case fromJSON jsonNode of
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
logFailTemp = logFailUsing $ do
  tmp <- (++ "/graphula") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmp
  openTempFile tmp "fail-.graphula"

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

  -- | Specify the method for resolving a node's key
  --
  -- This can be
  --
  -- @
  -- 'GenerateKey 'Default   -- automatically generate keys from the database
  -- 'GenerateKey 'Arbitrary -- automatically generate keys using @'Arbitrary'@
  -- 'SpecifyKey             -- explicitly pass keys to @'node'@
  -- @
  --
  -- Most types will use @'GenerateKey Default'@ or @'GenerateKey Arbitrary'@.
  -- Only use @'SpecifyKey'@ if the key for a value is always defined
  -- externally.
  type KeySource a :: KeySourceType
  type instance KeySource a = 'GenerateKey 'Default

  -- | Assign values from the 'Dependencies' collection to a value.
  -- 'dependsOn' must be an idempotent operation.
  --
  -- Law:
  --
  -- prop> (\x d -> x `dependsOn` d `dependsOn` d) = dependsOn
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

-- | Source of keys
data KeySourceType
  = GenerateKey KeyStrategy
  -- ^ Generate keys using the specified @'Strategy'@
  | SpecifyKey
  -- ^ Always explicitly pass an external key

-- | How to implicitly generate keys
data KeyStrategy
  = Default
  -- ^ Let the database generate keys
  | Arbitrary
  -- ^ Use '@Arbitrary'@ to generate keys

data GenerationFailure
  = GenerationFailureMaxAttemptsToConstrain TypeRep
  -- ^ Could not satisfy constraints defined using @'suchThat'@
  | GenerationFailureMaxAttemptsToInsert TypeRep
  -- ^ Could not satisfy database constraints on insert
  deriving (Show, Typeable, Eq)

instance Exception GenerationFailure

type GraphulaNode m a
  = ( Generate m a
    , HasDependencies a
    , Logging m a
    , PersistEntityBackend a ~ SqlBackend
    , PersistEntity a
    , Typeable a
    )


{-|
  Generate a value with data dependencies. This leverages
  'HasDependencies' to insert the specified data in the generated value. All
  dependency data is inserted after any editing operations.

  > node @Dog (ownerId, veterinarianId) mempty
  > node @Dog (ownerId, veterinarianId) $ edit $ \dog ->
  >   dog {name = "fido"}

  A value that has an externally managed key will always require an extra argument

  > someKey <- generateKey
  > node @Cat (ownerId, veterinarianId) someKey mempty
  > anotherKey <- generateKey
  > node @Cat (ownerId, veterinarianId) anotherKey $ edit $ \cat ->
  >   cat {name = "milo"}
-}
node
  :: forall a s m r c
   . ( GraphulaContext m '[a]
     , KeySource a ~ s
     , ResolveArgs s
     , Args s m a ~ (Dict s c -> r)
     , c
     )
  => Dependencies a
  -> r
node deps = resolveArgs @s @a @m deps Dict

-- | Modify the node after it's been generated
--
-- @
-- a <- node @A () $ edit $ \a -> a { someField = True }
-- @
--
edit :: (a -> a) -> NodeOptions a
edit f = mempty { nodeOptionsEdit = Kendo $ Just . f }

-- | Require a node to satisfy the specified predicate
--
-- @
-- a <- node @A () $ suchThat $ (== True) . someField
-- @
--
suchThat :: (a -> Bool) -> NodeOptions a
suchThat f = mempty { nodeOptionsEdit = Kendo $ \a -> a <$ guard (f a) }

-- | Options for generating an individual node
--
-- @'NodeOptions'@ can be created and combined with the Monoidal
-- operations @'(<>)'@ and @'mempty'@.
--
-- @
-- a1 <- node @A () mempty
-- a2 <- node @A () $ edit $ \a -> a { someField = True }
-- a3 <- node @A () $ suchThat $ (== True) . someField
-- @
--
newtype NodeOptions a = NodeOptions
  { nodeOptionsEdit :: Kendo Maybe a
  }
  deriving (Generic)

instance Semigroup (NodeOptions a) where
  (<>) = gmappend
  {-# INLINE (<>) #-}

instance Monoid (NodeOptions a) where
  mempty = gmempty
  {-# INLINE mempty #-}

-- | Like @'Endo'@ but uses Kliesli composition
newtype Kendo m a = Kendo { appKendo :: a -> m a }

instance Monad m => Semigroup (Kendo m a) where
  Kendo f <> Kendo g = Kendo $ f <=< g
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Kendo m a) where
  mempty = Kendo pure
  {-# INLINE mempty #-}

-- | Define arguments to @'node'@ for each @'KeySourceType'@
--
-- The @'Dict'@ argument has two functions
-- * Carries around any constraints that the corresponding
--   @'ResolveArgs@' instance will need
-- * Differentiates the result type enough that this type family can be
--   injective
--
type family Args s m a = r | r -> s where
  Args 'SpecifyKey m a
    = Dict 'SpecifyKey ()
    -> Key a
    -- ^ Key must be explicitly provided
    -> NodeOptions a
    -> m (Entity a)

  Args ('GenerateKey 'Arbitrary) m a
    = Dict ('GenerateKey 'Arbitrary) (Arbitrary (Key a))
    -- ^ Key will be generated using @'Arbitrary'@
    -> NodeOptions a
    -> m (Entity a)

  Args ('GenerateKey 'Default) m a
    = Dict ('GenerateKey 'Default) ()
    -- ^ Key will come from database
    -> NodeOptions a
    -> m (Entity a)

-- | Pass a tagged constraint at the term level
data Dict (t :: k) (c :: Constraint) where
  Dict ::c => Dict t c

-- | Resolve arguments for a specific instantiation of @'node'@
class ResolveArgs (s :: KeySourceType) where
  resolveArgs
    :: forall a m
     . GraphulaContext m '[a]
    => Dependencies a
    -> Args s m a

instance ResolveArgs 'SpecifyKey where
  resolveArgs
    :: forall a m
     . GraphulaContext m '[a]
    => Dependencies a
    -> Args 'SpecifyKey m a
--  -> Dict 'SpecifyKey ()
--  -> Key a
--  -> NodeOptions a
--  -> m (Entity a)
  resolveArgs deps Dict key o = nodeImpl deps (Just $ pure key) o

instance ResolveArgs ('GenerateKey 'Default) where
  resolveArgs
    :: forall a m
     . GraphulaContext m '[a]
    => Dependencies a
    -> Args ( 'GenerateKey 'Default) m a
--  -> Dict ( 'GenerateKey 'Default) ()
--  -> NodeOptions a
--  -> m (Entity a)
  resolveArgs deps Dict o = nodeImpl deps Nothing o

instance ResolveArgs ('GenerateKey 'Arbitrary) where
  resolveArgs
    :: forall a m
     . GraphulaContext m '[a]
    => Dependencies a
    -> Args ( 'GenerateKey 'Arbitrary) m a
--  -> Dict ( 'GenerateKey 'Arbitrary) (Arbitrary (Key a))
--  -> NodeOptions a
--  -> m (Entity a)
  resolveArgs deps Dict o = nodeImpl deps (Just arbitrary) o

nodeImpl
  :: forall a m
   . GraphulaContext m '[a]
  => Dependencies a
  -> Maybe (Gen (Key a))
  -> NodeOptions a
  -> (m (Entity a))
nodeImpl dependencies mGenKey NodeOptions {..} = attempt 100 10 $ do
  initial <- generateNode
  for (appKendo nodeOptionsEdit initial) $ \edited -> do
    let hydrated = edited `dependsOn` dependencies
    logNode hydrated
    mKey <- traverse (liftIO . generate) mGenKey
    pure (mKey, hydrated)

attempt
  :: forall a m
   . (Typeable a, GraphulaContext m '[a])
  => Int
  -> Int
  -> m (Maybe (Maybe (Key a), a))
  -> m (Entity a)
attempt maxEdits maxInserts source = loop 0 0
 where
  loop :: Int -> Int -> m (Entity a)
  loop numEdits numInserts
    | numEdits >= maxEdits = die GenerationFailureMaxAttemptsToConstrain
    | numInserts >= maxInserts = die GenerationFailureMaxAttemptsToInsert
    | otherwise = source >>= \case
      Nothing -> loop (succ numEdits) numInserts
      Just (mKey, value) -> insert mKey value >>= \case
        Nothing -> loop (succ numEdits) (succ numInserts)
        Just a -> pure a

  die :: (TypeRep -> GenerationFailure) -> m (Entity a)
  die e = throwIO $ e $ typeRep (Proxy :: Proxy a)

-- | For entities that only have singular 'Dependencies'. It uses data instead
-- of newtype to match laziness of builtin tuples.
data Only a = Only { fromOnly :: a }
  deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

only :: a -> Only a
only = Only
