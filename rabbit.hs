-- Rabbit OSX Application Manager v1.0
-- Cody Faust <cfaust244@gmail.com>
-- Uses split from cabal

module Main where
import System.Environment
import System.IO
import System.Directory
import Data.List.Split
import Data.Maybe
import Utilities
import FileIO
import SystemLevel


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
              | x == "remove"  = uninstall (head xs)
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
                putStrLn ("Package version is: " ++ version)
                downloadPackage xs              -- TODO handle errors with these
                extractAndInstallPackage xs
                writeInstalled (xs ++ ":" ++ version)
                putStrLn (xs ++ " was a tasty carrot!")


-- Verifies it can uninstall the given applications, and then calls the necessary 
-- functions to perform such operations 
uninstall :: String -> IO()        
uninstall []  = putStrLn "Please enter what to uninstall"
uninstall xs  = do putStrLn ("Uninstalling " ++  xs)
                   isIns <- isInstalled xs
                   if isIns
                     then do removePackage xs
                             putStrLn (xs ++ " was successfully removed, the rabbit is sad")
                     else putStrLn "That package isn't installed"



-- Just prints out a simple help menu
-- TODO: Clean up help menu
help :: IO()
help = do putStrLn "install application... installs the application or applications supplied"
          putStrLn "update... updates all installed packages"
          putStrLn "remove application...removes the supplied application or applications"



-- Lists all carrots availible
listAvailible :: IO String -> IO()
listAvailible contents = do result <- contents
                            putStrLn result


-- See if the package is availible 
isAvailible :: String -> IO Bool
isAvailible toCheck =  do x <- readAvailible
                          if elem toCheck (dropEveryOther (words (unwords (splitOn ":" x)))) 
                           then return True 
                           else return False


-- Lists all installed carrots
listInstalled :: IO String -> IO()
listInstalled contents = do result <- contents
                            putStrLn result



-- See if the package is installed
isInstalled :: String -> IO Bool
isInstalled toCheck =  do check <- doesFileExist "installed.list"
                          if check 
                            then do x <- readInstalled
                                    if elem toCheck (dropEveryOther (words (unwords (splitOn ":" x))))
                                        then return True
                                        else return False
                                      else return False

                         

-- Returns a tuple of availible carrots with (Name, Version)
getTupleAvail :: IO [(String, String)]
getTupleAvail = do avail <- readAvailible
                   return $ zip (dropEveryOther (splitOneOf ":'\n'" avail)) (dropEveryOther' (splitOneOf ":'\n'" avail))



-- Returns a tuple of installed carrots with (Name, Version)
getTupleInstalled :: IO [(String, String)]
getTupleInstalled = do avail <- readInstalled
                       return $ zip (dropEveryOther (splitOneOf ":'\n'" avail)) (dropEveryOther' (splitOneOf ":'\n'" avail))



-- Removes the Just from infront of a value so that it can be used easier
removeJust :: Monad m => m (Maybe b) -> m b
removeJust x = do value <- x
                  let finally = fromJust value 
                  return finally


-- Takes the availible package list which is an IO [(String, String)], "unpacks" it
-- Sends it to findPackageVersion and wraps the result back in an IO (Maybe String)
findWrapper :: String -> IO (Maybe String)
findWrapper x = do list <- getTupleAvail
                   return (findPackageVersion x list)

--  Takes a List of Tuples and searches for a match and returns the value (version) attached (like a dictionary)
findPackageVersion :: (Eq p) => p -> [(p,v)] -> Maybe v  
findPackageVersion package [] = Nothing  
findPackageVersion package ((p,v):xs) = if package == p  
                                        then Just v  
                                        else findPackageVersion package xs
