{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_arithmetic (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\bin"
libdir     = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\lib\\x86_64-windows-ghc-9.0.2\\arithmetic-0.0.0-CvrcPM3MfhFBsR4RqOrqhK"
dynlibdir  = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\share\\x86_64-windows-ghc-9.0.2\\arithmetic-0.0.0"
libexecdir = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\libexec\\x86_64-windows-ghc-9.0.2\\arithmetic-0.0.0"
sysconfdir = "E:\\Master\\AP\\Assigment1\\part2\\.stack-work\\install\\860c298e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arithmetic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arithmetic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "arithmetic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "arithmetic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arithmetic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arithmetic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
