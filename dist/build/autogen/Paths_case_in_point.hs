module Paths_case_in_point (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/daniel/bin"
libdir     = "/Users/daniel/lib/case-in-point-0.1/ghc-7.0.4"
datadir    = "/Users/daniel/share/case-in-point-0.1"
libexecdir = "/Users/daniel/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "case_in_point_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "case_in_point_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "case_in_point_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "case_in_point_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
