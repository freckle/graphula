{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_graphula_persistent (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/bin"
libdir     = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/lib/x86_64-linux-ghc-8.2.2/graphula-persistent-0.1.0-4e7yTwa5KSn8T6sZnXQlEO"
dynlibdir  = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/share/x86_64-linux-ghc-8.2.2/graphula-persistent-0.1.0"
libexecdir = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/libexec/x86_64-linux-ghc-8.2.2/graphula-persistent-0.1.0"
sysconfdir = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphula_persistent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphula_persistent_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphula_persistent_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphula_persistent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphula_persistent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphula_persistent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
