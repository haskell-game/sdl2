module Paths_sdl2 (
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
version = Version {versionBranch = [1,1,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chris/Projects/sdl2/.hsenv/cabal/bin"
libdir     = "/home/chris/Projects/sdl2/.hsenv/cabal/lib/x86_64-linux-ghc-7.8.2/sdl2-1.1.2"
datadir    = "/home/chris/Projects/sdl2/.hsenv/cabal/share/x86_64-linux-ghc-7.8.2/sdl2-1.1.2"
libexecdir = "/home/chris/Projects/sdl2/.hsenv/cabal/libexec"
sysconfdir = "/home/chris/Projects/sdl2/.hsenv/cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sdl2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sdl2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sdl2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sdl2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sdl2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
