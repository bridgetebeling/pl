{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_MiniRacket (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/bin"
libdir     = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/lib/aarch64-osx-ghc-8.10.7/MiniRacket-0.1.0.0-ITm9cvaW8c98GdFvxOLu3J"
dynlibdir  = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/share/aarch64-osx-ghc-8.10.7/MiniRacket-0.1.0.0"
libexecdir = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/libexec/aarch64-osx-ghc-8.10.7/MiniRacket-0.1.0.0"
sysconfdir = "/Users/bridgetebeling/Downloads/MiniRacketProject/.stack-work/install/aarch64-osx/3353aca280a922b1b9f1d760602ee3fa9682f58e556bce65938f7803f6aab976/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MiniRacket_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MiniRacket_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "MiniRacket_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "MiniRacket_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MiniRacket_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MiniRacket_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
