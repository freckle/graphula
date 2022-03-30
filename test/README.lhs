# Graphula

Graphula is a simple interface for generating persistent data and linking its
dependencies. We use this interface to generate fixtures for automated testing.


<!--
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (module Main) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Graphula
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.Markdown.Unlit ()

instance (ToBackendKey SqlBackend a) => Arbitrary (Key a) where
  arbitrary = toSqlKey <$> arbitrary
```
-->

## Arbitrary Data

Graphula utilizes `QuickCheck` to generate random data. We need to declare
`Arbitrary` instances for our models.

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
School
  name String
  deriving Show Eq Generic

Teacher
  schoolId SchoolId
  name String
  deriving Show Eq Generic

Course
  schoolId SchoolId
  teacherId TeacherId
  name String
  deriving Show Eq Generic

Student
  name String
  deriving Show Eq Generic

Question
  content String
  deriving Show Eq Generic

Answer
  questionId QuestionId
  studentId StudentId
  yes Bool
  UniqueAnswer questionId studentId
  deriving Show Eq Generic
|]

instance Arbitrary School where
  arbitrary = genericArbitrary

instance Arbitrary Teacher where
  arbitrary = genericArbitrary

instance Arbitrary Course where
  arbitrary = genericArbitrary

instance Arbitrary Student where
  arbitrary = genericArbitrary

instance Arbitrary Question where
  arbitrary = genericArbitrary

instance Arbitrary Answer where
  arbitrary = genericArbitrary
```

## Dependencies

We declare dependencies via the `HasDependencies` typeclass and its associated
type `Dependencies`. If a model does not have any dependencies, we only need to
declare an empty instance.

```haskell
instance HasDependencies School

instance HasDependencies Student

instance HasDependencies Question
```

For single-dependency models, we use the `Only` type.

```haskell
instance HasDependencies Teacher where
  type Dependencies Teacher = Only SchoolId
```

Multi-dependency models use tuples. Declare these dependencies in the order they
appear in the model's type definition. `HasDependencies` leverages generic
programming to inject dependencies for you.

```haskell
instance HasDependencies Course where
  type Dependencies Course = (SchoolId, TeacherId)

instance HasDependencies Answer where
  type Dependencies Answer = (QuestionId, StudentId)
```

## Logging failures

`runGraphulaLogged` will dump generated data to a temporary file. Or
`runGraphulaLoggedWithFileT` can be used to pass an explicit path.

```haskell
loggingSpec :: IO ()
loggingSpec = do
  let
    logFile :: FilePath
    logFile = "test.graphula"

    failingGraph :: IO ()
    failingGraph = runGraphulaT Nothing runDB . runGraphulaLoggedWithFileT logFile $ do
      student <- node @Student () mempty
      question <- node @Question () mempty
      answer <- node @Answer
        (entityKey question, entityKey student)
        $ edit $ \a -> a { answerYes = True }

      -- Test failures will cause the graph to be logged (not any exception)
      liftIO $ answerYes (entityVal answer) `shouldBe` False

  failingGraph `shouldThrow` anyException

  n <- lines <$> readFile logFile
  n `shouldSatisfy` (not . null)
```

## Running It

```haskell
simpleSpec :: IO ()
simpleSpec =
  runGraphulaT Nothing runDB $ do
    school <- node @School () mempty
    teacher <- node @Teacher (Only $ entityKey school) mempty
    course <- node @Course (entityKey school, entityKey teacher) mempty
    student <- node @Student () $ edit $ \s -> s { studentName = "Pat" }
    question <- node @Question () mempty
    answer <- node @Answer
      (entityKey question, entityKey student)
      $ edit $ \a -> a { answerYes = True }

    liftIO $ do
      -- Typically, you would run some other function like "fetch correct
      -- answers at school" and assert you found the correct answers you
      -- generated. In this example we just assert some things about the data
      -- directly:
      teacherSchoolId (entityVal teacher) `shouldBe` entityKey school
      courseTeacherId (entityVal course) `shouldBe` entityKey teacher
      answerYes (entityVal answer) `shouldBe` True
```

<!--
```haskell
main :: IO ()
main = hspec $
  describe "graphula" . parallel $ do
    it "generates and links arbitrary graphs of data" simpleSpec
    it "allows logging graphs" loggingSpec

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "test.db" $ do
  runMigration migrateAll
  f
```
-->
