module SystemLevel
( downloadMaster
, downloadPackage
, downloadPackageList
, extractAndInstallPackage
, removePackage
) where

import GHC.IO.Exception
import System.Cmd
import System.Directory

-- Download the master package list
downloadMaster :: IO ExitCode
downloadMaster = do tmp <- getTemporaryDirectory
                    setCurrentDirectory tmp
                    rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/carrots.list"]

-- Utilize curl which is included with OSX to download repository data
downloadPackage :: String -> IO ExitCode
downloadPackage package = do tmp <- getTemporaryDirectory
                             setCurrentDirectory tmp
                             rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".tar.bz2"]

-- Downloads the config file for the requested package -- for update operations
downloadPackageList :: String -> IO ExitCode
downloadPackageList package = do tmp <- getTemporaryDirectory
                                 setCurrentDirectory tmp
                                 rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".list"]

-- Extract the package from .tar.bz2 and place in /Applications
extractAndInstallPackage :: String -> IO ExitCode
extractAndInstallPackage package = do tmp <- getTemporaryDirectory
                                      rawSystem "tar" ["-jxf", tmp ++ package ++ ".tar.bz2", "-C", "/Applications"]

-- Remove an installed package
removePackage :: String -> IO ExitCode
removePackage package = do setCurrentDirectory "/Applications"   -- OSX specific
                           rawSystem "rm" ["-rf", package ++ ".app"]



                             

