module SystemLevel
( downloadMaster
, downloadPackage
, describe
, extractAndInstallPackage
, deletePackage
, setRabbitDir
, setTmpDir
) where

import GHC.IO.Exception
import System.Cmd
import System.Directory

-- System level commands should always return the directory to the RabbitDir when finished 
downloadMaster :: IO ExitCode
downloadMaster = rawSystem "curl" ["-s", "-O", "http://www.rabbitbyte.org/repo/carrots.list"]
                 

-- Utilize curl which is included with OSX to download repository data
downloadPackage :: String -> IO ()
downloadPackage package = do setTmpDir
                             rawSystem "curl" ["-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".tar.bz2"]
                             setRabbitDir

-- Extract the package from .tar.bz2 and place in /Applications
extractAndInstallPackage :: String -> IO ()
extractAndInstallPackage package = do setTmpDir
                                      rawSystem "tar" ["-jxf", package ++ ".tar.bz2", "-C", "/Applications"]
                                      setRabbitDir

-- Remove an installed package
deletePackage :: String -> IO ()
deletePackage package = do setCurrentDirectory "/Applications"
                           rawSystem "rm" ["-rf", package ++ ".app"]
                           setRabbitDir

describe :: String -> IO ()
describe package = do setTmpDir
                      rawSystem "curl" ["-s", "-O", "http://www.rabbitbyte.org/repo/" ++ package ++ "/" ++ package ++ ".list"]
                      contents <- readFile (package ++ ".list")
                      putStr "\n"
                      putStrLn contents
                      putStr "\n"
                      setRabbitDir


setRabbitDir :: IO()
setRabbitDir = do home <- getHomeDirectory
                  setCurrentDirectory (home ++ "/.rabbit")
                  return ()

setTmpDir :: IO()
setTmpDir = do tmp <- getTemporaryDirectory
               setCurrentDirectory tmp
               return ()