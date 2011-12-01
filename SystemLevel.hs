module SystemLevel
( downloadMaster
, downloadPackage
, downloadPackageList
, extractAndInstallPackage
) where

import GHC.IO.Exception
import System.Cmd
import System.Directory

-- Download the master package list
downloadMaster :: IO ExitCode
downloadMaster = do setCurrentDirectory "/private/var/tmp/"
                    rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/carrots.list"]

-- Utilize curl which is included with OSX to download repository data
downloadPackage :: String -> IO ExitCode
downloadPackage package = do setCurrentDirectory "/private/var/tmp/"
                             rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".tar.bz2"]

-- Downloads the config file for the requested package -- for update operations
downloadPackageList :: String -> IO ExitCode
downloadPackageList package = do setCurrentDirectory "/private/var/tmp/"
                                 rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".list"]

-- Extract the package from .tar.bz2 and place in /Applications
extractAndInstallPackage :: String -> IO GHC.IO.Exception.ExitCode
extractAndInstallPackage package = rawSystem "tar" ["-jxvf", "/private/var/tmp/" ++ package ++ ".tar.bz2", "-C", "/Applications"]