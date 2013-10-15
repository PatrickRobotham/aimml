module Paths_aimml (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/patrick/.cabal/bin"
libdir     = "/Users/patrick/.cabal/lib/aimml-0.1/ghc-7.4.2"
datadir    = "/Users/patrick/.cabal/share/aimml-0.1"
libexecdir = "/Users/patrick/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "aimml_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aimml_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "aimml_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aimml_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
