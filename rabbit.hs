-- Rabbit OSX Application Manager v1.0
-- Cody Faust <cfaust244@gmail.com>
-- Uses split from cabal

module Main where

import System.Environment
import System.IO
import Data.List.Split
import Data.Maybe


-- main, just gets the command line args and sends them to the clController
main :: IO()
main = do args <- getArgs
          clController args



-- Decides what action to perform based on the arguments given
-- TODO: Allow for the user to install more than one carrot at a time, it currently discards any extras
clController :: [String] -> IO()
clController []   = putStrLn "Enter a command please!"
clController (x:xs)
              | x == "install" = do installed <- (isInstalled (head xs))
                                    availible <- (isAvailible (head xs))
                                    if installed 
                                     then putStrLn "This package is already installed" 
                                     else if availible
                                           then install (head xs)
                                           else putStrLn "Sorry but the requested package isn't availible"
              | x == "update"  = putStrLn "Updating..."
              | x == "remove"  = uninstall xs
              | x == "help"    = help
              | x == "list-all"= listAvailible $ readAvailible
              | x == "list-installed" = listInstalled $ readInstalled
              | otherwise = putStrLn "Unknown Command"



-- Verifies it can install the given applications, and then calls the necessary 
-- functions to perform such operations 
install :: String -> IO()        
install [] = putStrLn "Please enter what to install"
install xs = do putStrLn ("Installing " ++ xs)
                version <- removeJust $ findWrapper xs
                putStrLn ("Version is: " ++ version)



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
readAvailible :: IO String
readAvailible =  do contents <- readFile "carrots.list"
                    return contents



-- Reads the installed.list file and returns an IO String for whatever needs it
readInstalled :: IO String
readInstalled =  do contents <- readFile "installed.list"
                    return contents



-- Lists all carrots availible
listAvailible :: IO String -> IO()
listAvailible contents = do result <- contents
                            putStrLn result


-- Lists all installed carrots
listInstalled :: IO String -> IO()
listInstalled contents = do result <- contents
                            putStrLn result



-- See if the package is installed
isInstalled :: String -> IO Bool
isInstalled toCheck =  do x <- readInstalled
                          if elem toCheck (dropEveryOther (words (unwords (splitOn ":" x)))) 
                           then return True 
                           else return False                                      
                                                                    


-- See if the package is availible 
isAvailible :: String -> IO Bool
isAvailible toCheck =  do x <- readAvailible
                          if elem toCheck (dropEveryOther (words (unwords (splitOn ":" x)))) 
                           then return True 
                           else return False



-- Function to remove every other element from a list, used to remove the version when needed
dropEveryOther :: [a] -> [a]
dropEveryOther [] = []
dropEveryOther (x:xs)
                | null xs = x:[]
                | otherwise = x:(dropEveryOther (tail xs))
               


-- This drops every other starting with the head, instead of the second element                                    
dropEveryOther' :: [a] -> [a]
dropEveryOther' [] = []
dropEveryOther' (x:xs)
                 | null xs = []
                 | otherwise = (head xs):dropEveryOther' (tail xs)

                         

-- Returns a tuple of availible carrots with (Name, Version)
getTupleAvail :: IO [(String, String)]
getTupleAvail = do avail <- readAvailible
                   return $ zip (dropEveryOther (splitOneOf ":'\n'" avail)) (dropEveryOther' (splitOneOf ":'\n'" avail))



-- Returns a tuple of installed carrots with (Name, Version)
getTupleInstalled :: IO [(String, String)]
getTupleInstalled = do avail <- readInstalled
                       return $ zip (dropEveryOther (splitOneOf ":'\n'" avail)) (dropEveryOther' (splitOneOf ":'\n'" avail))



-- Make it cleaner to send to putStrLn
removeJust :: Monad m => m (Maybe b) -> m b
removeJust x = do value <- x
                  let finally = fromJust value 
                  return finally

-- The monad cannot be escaped! (Muhahaha?)
findWrapper :: String -> IO (Maybe String)
findWrapper x = do list <- getTupleAvail
                   return (findPackageVersion x list)

-- 
findPackageVersion :: (Eq p) => p -> [(p,v)] -> Maybe v  
findPackageVersion package [] = Nothing  
findPackageVersion package ((p,v):xs) = if package == p  
                                        then Just v  
                                        else findPackageVersion package xs