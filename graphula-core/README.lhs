# Graphula Core

Graphula is a simple interface for generating data and linking its dependencies. We use this interface to generate fixtures for automated testing. The interface is extensible and supports pluggable front-ends.


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
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Simple graphula" $
    it "builds arbitrary graphs of data" $
      runGraphula graphIdentity $ do
        -- Declare the graph at the term level
        Identity a <- node @A
        Identity b <- nodeWith @B (only a)
        -- Type application is not necessary, but recommended for clarity.
        Identity c <- nodeEditWith @C (a, b) $ \n ->
          n { cc = "spanish" }

        -- Do something with your data
        liftIO $ do
          cc c `shouldBe` "spanish"
          ca c `shouldBe` ba b
```

## The Data

```haskell
data A = A { aa :: String, ab :: Int }
  deriving (Show, Eq, Generic)

data B = B { ba :: A, bb :: String }
  deriving (Show, Eq, Generic)

data C = C { ca :: A, cb :: B , cc :: String}
  deriving (Show, Eq, Generic)
```

## Arbitrary

Graphula utilizes `QuickCheck` to generate random data. We need to declare `Arbitrary` instances for our types.

```haskell
instance Arbitrary C where
  arbitrary = C <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary B where
  arbitrary = B <$> arbitrary <*> arbitrary

instance Arbitrary A where
  arbitrary = A <$> arbitrary <*> arbitrary
```

## Dependencies

We declare dependencies via the `HasDependencies` typeclass and its associated type `Dependencies`.

By default a type does not have any dependencies. We only need to decalare an empty instance

```haskell
instance HasDependencies A
```

For single dependencies we use the `Only` type.

```haskell
instance HasDependencies B where
  type Dependencies B = Only A
```

Groups of dependencies use tuples. Declare these dependencies in the order they appear in the type. `HasDependencies` leverages generic programming to inject dependencies for you.

```haskell
instance HasDependencies C where
  type Dependencies C = (A, B)
```

## Serialization

We use `JSON` as a human readable serialization format. Graphula dumps graphs to a temp file on test failure. You can inspect or `runGraphulaReplay` a failed graph for red/green refactor.

```haskell
instance ToJSON A
instance FromJSON A

instance ToJSON B
instance FromJSON B

instance ToJSON C
instance FromJSON C
```

## Running It

`runGraphula` requires you to provide a front-end. This carries the instructions for evaluating a graph. Our simple `Frontend` is not constraining types and it is wrapping insert results in `Identity`. `Graphula.Persist` is an example of a more complex frontend utilizing `Database.Persist`.

```haskell
graphIdentity :: Frontend NoConstraint Identity (IO r) -> IO r
graphIdentity f = case f of
  Insert n next ->
    next $ Just $ Identity n
```
