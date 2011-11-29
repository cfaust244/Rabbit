-- Rabbit OSX Application Manager v1.0
-- Cody Faust <cfaust244@gmail.com>
-- Uses split from cabal

module Main where

import System.Environment
import System.IO
import Data.List.Split

-- main, just gets the command line args and sends them to the clController
main :: IO()
main = do args <- getArgs
          clController args


-- Decides what action to perform based on the arguments given
clController :: [String] -> IO()
clController []   = putStrLn "Enter a command please!"
clController (x:xs)
              | x == "install" = install xs
              | x == "update"  = putStrLn "Updating..."
              | x == "remove"  = uninstall xs
              | x == "help"    = help
              | x == "list-all"= listCarrots $ readCarrots
              | otherwise = putStrLn "Unknown Command"

-- Verifies it can install the given applications, and then calls the necessary 
-- functions to perform such operations 
install :: [String] -> IO()        
install [] = putStrLn "Please enter what to install"
install xs = putStrLn ("Installing " ++ (unwords xs))


-- Verifies it can uninstall the given applications, and then calls the necessary 
-- functions to perform such operations 
uninstall :: [String] -> IO()        
uninstall [] = putStrLn "Please enter what to uninstall"
uninstall xs  = putStrLn ("Uninstalling " ++ (unwords xs))

-- Just prints out a simple help menu
-- TODO: Clean up help menu
help :: IO()
help = do putStrLn "install application... installs the application or applications supplied"
          putStrLn "update... updates all installed packages"
          putStrLn "remove application...removes the supplied application or applications"

-- Reads the carrots.list file and returns an IO String for whatever needs it
-- TODO: Make more generic so it works on more than one file? 
readCarrots :: IO String
readCarrots =  do contents <- readFile "carrots.list"
                  return contents

-- Reads the installed.list file and returns an IO String for whatever needs it
readInstalled :: IO String
readInstalled =  do contents <- readFile "installed.list"
                    return contents

-- Lists all carrots availible
listCarrots :: IO String -> IO()
listCarrots contents = do result <- contents
                          putStrLn result
                                
-- See if the package is installed
isInstalled :: IO String -> String -> IO Bool
isInstalled areInstalled toCheck =  do x <- areInstalled
                                       if elem toCheck (dropEveryOther (words (unwords (splitOn ":" x)))) then return True else return False

-- Function to remove every other element from a list, used to remove the version when needed
dropEveryOther :: [a] -> [a]
dropEveryOther [] = []
dropEveryOther (x:xs)
                | null xs = x:[]
                | otherwise = x:(dropEveryOther (tail xs))
                                      
