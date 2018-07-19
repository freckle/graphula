{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_graphula_core (
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
version = Version [0,3,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/bin"
libdir     = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/lib/x86_64-linux-ghc-8.2.2/graphula-core-0.3.0-KB0qlZWGUfxDh2xuSDfPnR"
dynlibdir  = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/share/x86_64-linux-ghc-8.2.2/graphula-core-0.3.0"
libexecdir = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/libexec/x86_64-linux-ghc-8.2.2/graphula-core-0.3.0"
sysconfdir = "/home/patrick/code/frontrowed/megarepo/haskell/.stack-work-lts/install/x86_64-linux/lts-11.17/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphula_core_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphula_core_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphula_core_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphula_core_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphula_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphula_core_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
