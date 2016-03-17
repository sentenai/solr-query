module Paths_lucene_query (
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

bindir     = "/home/mitchell/sentenai/lucene-query/.stack-work/install/x86_64-linux/lts-5.8/7.10.3/bin"
libdir     = "/home/mitchell/sentenai/lucene-query/.stack-work/install/x86_64-linux/lts-5.8/7.10.3/lib/x86_64-linux-ghc-7.10.3/lucene-query-0.1.0.0-DiSON5gFfTo6LCDmmQiT4o"
datadir    = "/home/mitchell/sentenai/lucene-query/.stack-work/install/x86_64-linux/lts-5.8/7.10.3/share/x86_64-linux-ghc-7.10.3/lucene-query-0.1.0.0"
libexecdir = "/home/mitchell/sentenai/lucene-query/.stack-work/install/x86_64-linux/lts-5.8/7.10.3/libexec"
sysconfdir = "/home/mitchell/sentenai/lucene-query/.stack-work/install/x86_64-linux/lts-5.8/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lucene_query_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lucene_query_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lucene_query_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lucene_query_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lucene_query_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
