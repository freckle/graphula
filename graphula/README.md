# Graphula Core

The core of graphula is a simple interface for declaring data and linking its dependencies.


```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Aeson
import Data.Functor.Identity (Identity(..))
import Control.Monad.IO.Class
import Graphula
import GHC.Generics (Generic)
import Test.QuickCheck

main :: IO ()
main =
  -- wrap computation to allow shrinking, saving failed states, replay, etc with a free monad
  runGraphula graphIO $ do
    -- Declare the graph at the term level
    Identity a <- node @A
    Identity b <- nodeWith @B (only a) -- This can actually be inferred
    Identity c <- nodeEditWith @C (a, b) $ \n ->
      n { cc = "spanish" }

    -- do shit with it
    liftIO $ print (a, b, c)
```

## The Data

```haskell
data A = A { aa :: String, ab :: Int }
  deriving (Show, Generic)

data B = B { ba :: A, bb :: String }
  deriving (Show, Generic)

data C = C { ca :: A, cb :: B , cc :: String}
  deriving (Show, Generic)
```

## Arbitrary

Graphula utilizes `QuickCheck` to randomly generate data. So we need to declare `Arbitrary` instances.

```haskell
instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary
```

## Dependencies

Dependencies are declared via the `HasDependencies` typeclass.

By default a type does not have any dependencies, so we only need to decalre a blank instance

```haskell
instance HasDependencies A
```

To declare a single dependency we use an associated type with `Only`.

```haskell
instance HasDependencies B where
  type Dependencies B = Only A
```

Groups of dependencies can be declared with tuples. These dependencies should be declared in the order they appear in the type. This allows us to leverage generic programming to write the rest of the instance for you.

```haskell
instance HasDependencies C where
  type Dependencies C = (A, B)
```

## Serialization

Graphula uses JSON as a human readable serialization format. When a test fails the graph will be dumped to a test file so you can inspect it or replay it for red/green refact with `runGraphulaReplay`.

```haskell
instance ToJSON A
instance FromJSON A

instance ToJSON B
instance FromJSON B

instance ToJSON C
instance FromJSON C
```

## Running It

`runGraphula` requires you to pass it a frontend. This provides the instructions for evaluating a graph. This simple `Frontend` is not constraining our types an is wrapping them in an `Identity` type. `Graphula.Persist` is an example of a more complex frontend utilizing `Database.Persist`.

```haskell
graphIO :: Frontend NoConstraint Identity (IO r) -> IO r
graphIO f = case f of
  Insert n next ->
    next $ Just $ Identity n
```
