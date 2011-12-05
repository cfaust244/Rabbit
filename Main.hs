-- Rabbit OSX Application Manager v1.0
-- Cody Faust <cfaust244@gmail.com>

module Main where
import System.Environment
import System.IO
import System.Directory
import Data.List.Split
import Data.Maybe
import FileIO

-- Known to be needed
import RabbitParse
import Text.Parsec.Error
import SystemLevel
import qualified Data.Map as M


-- main, just gets the command line args and sends them to the clController
main :: IO()
main = do args <- getArgs
          clController args



-- Decides what action to perform based on the arguments given
-- TODO: Allow for the user to install more than one carrot at a time, it currently discards any extras
clController :: [String] -> IO()
clController []   = putStrLn "Enter a command please!"
clController (x:xs)
              | x == "install"  = install (head xs)

-- TODO make sure package isn't installed!
install :: String -> IO()
install package  = do ins <- isInstalled package
                      avl <- isAvailible package
                      case () of _
                                  | ins == True  -> putStrLn "Package is already installed!"
                                  | avl == False -> putStrLn "Package isn't availible!"
                                  | otherwise -> do putStrLn ("Package name: " ++ package)
                                                    downloadPackage package
                                                    extractAndInstallPackage package
                                                    return ()

isInstalled :: String -> IO Bool
isInstalled package = do check <- doesFileExist "installed.list"
                         case check of
                              True  -> do a <- handleErrors $ readSource "installed.list"
                                          if M.null a
                                              then return False
                                              else return $ M.member package a
                              False -> return False

isAvailible :: String -> IO Bool
isAvailible package = do downloadMaster
                         tmp <- getTemporaryDirectory
                         setCurrentDirectory tmp
                         check <- doesFileExist "carrots.list"
                         case check of
                            True -> do a <- handleErrors $ readSource "carrots.list"
                                       if M.null a 
                                           then return False
                                           else return $ M.member package a
                            False -> return False


handleErrors :: IO (Either ParseError Packages) -> IO Packages
handleErrors xs = do x <- xs
                     case x of
                              Left x   -> do putStrLn (show x)
                                             return M.empty
                              Right x  ->    return $ x