{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphula.AsJSON
    ( DumpJSON
    , dumpJSON
    , LoadJSON
    , loadJSON
    )
where

import Prelude

import Data.Aeson
import Data.Aeson.Types (parse)
import GHC.Generics

newtype AsJSON a = AsJSON { unAsJSON :: a }
    deriving stock (Eq, Show, Generic)

class (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => DumpJSON a
instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => DumpJSON a

instance DumpJSON a => ToJSON (AsJSON a) where
    toJSON = genericToJSON defaultOptions . unAsJSON
    toEncoding = genericToEncoding defaultOptions . unAsJSON

dumpJSON :: forall a . DumpJSON a => a -> Value
dumpJSON = toJSON . AsJSON

class (Generic a, GFromJSON Zero (Rep a)) => LoadJSON a
instance (Generic a, GFromJSON Zero (Rep a)) => LoadJSON a

instance LoadJSON a => FromJSON (AsJSON a) where
    parseJSON = fmap AsJSON . genericParseJSON defaultOptions

loadJSON :: forall a . LoadJSON a => Value -> Result a
loadJSON = fmap unAsJSON . parse parseJSON
