module Paths_sokal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nschoem/Documents/UChicago/yr4/cs22311/labs/sokal/.cabal-sandbox/bin"
libdir     = "/home/nschoem/Documents/UChicago/yr4/cs22311/labs/sokal/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/sokal-0.1.0.0"
datadir    = "/home/nschoem/Documents/UChicago/yr4/cs22311/labs/sokal/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/sokal-0.1.0.0"
libexecdir = "/home/nschoem/Documents/UChicago/yr4/cs22311/labs/sokal/.cabal-sandbox/libexec"
sysconfdir = "/home/nschoem/Documents/UChicago/yr4/cs22311/labs/sokal/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sokal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sokal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sokal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sokal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sokal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
