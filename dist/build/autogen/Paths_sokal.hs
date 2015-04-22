module Paths_sokal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/nschoem/.cabal/bin"
libdir     = "/home/nschoem/.cabal/lib/sokal-0.1.0.0/ghc-7.6.3"
datadir    = "/home/nschoem/.cabal/share/sokal-0.1.0.0"
libexecdir = "/home/nschoem/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sokal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sokal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sokal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sokal_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
