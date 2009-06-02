module Paths_serial (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/ross/.cabal/bin"
libdir     = "/Users/ross/.cabal/lib/serial-0.1/ghc-6.10.1"
datadir    = "/Users/ross/.cabal/share/serial-0.1"
libexecdir = "/Users/ross/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "serial_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "serial_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "serial_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "serial_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
