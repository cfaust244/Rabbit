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
              | x == "install"  = install $ head xs
              | x == "update"   = putStrLn "Updating..."
              | x == "remove"   = remove  $ head xs
              | x == "help"     = help
              | x == "list-a"   = listAvailible
              | x == "list-i"   = listInstalled
              | x == "describe" = describe $ head xs
              | otherwise = putStrLn "Unknown Command"



-- Verifies it can install the given applications, and then calls the necessary 
-- functions to perform such operations 
-- BTW the case () of _ is because I hate nested if's. They look terrible
install :: String -> IO()        
install [] = putStrLn "enter a package to install"
install xs = do installed <- isInstalled xs
                availible <- isAvailible xs
                case () of _
                            | installed == True -> putStrLn "This package is already installed"
                            | availible == True -> install' xs
                            | otherwise -> putStrLn "Sorry but the requested package isn't availible"


install' :: String -> IO()
install' xs = do putStrLn ("Installing " ++ xs)
                 version <- removeJust $ findWrapper xs
                 putStrLn ("Package version is: " ++ version)
                 downloadPackage xs                                -- TODO: Provide useful
                 extractAndInstallPackage xs                       -- error messages that
                 writeInstalled (xs ++ ":" ++ version)             -- are "less scary"
                 putStrLn (xs ++ " was a tasty carrot!")

-- Verifies it can remove the given package
-- then removes the application
-- TODO move "Removing package" till after it checks that the package is installed
remove     :: String -> IO()        
remove []  = putStrLn "Please enter what to remove"
remove xs  = do putStrLn ("Removing " ++  xs)
                isIns <- isInstalled xs
                if isIns
                  then do removePackage xs
                          removePackFromFile xs
                          putStrLn (xs ++ " was successfully removed, the rabbit is sad")
                  else putStrLn "That package isn't installed"



-- Just prints out a simple help menu
help :: IO()
help = do putStrLn "command: install  - followed by the package you wish to install"
          putStrLn "command: update   - updates all installed packages"
          putStrLn "command: remove   - followed by the package you wish to remove"
          putStrLn "command: list-a   - lists all availible applications"
          putStrLn "command: list-i   - lists all installed applications"



-- Lists all packages availible
listAvailible :: IO()
listAvailible = do result <- readAvailible
                   putStrLn result


-- See if the package is availible 
isAvailible :: String -> IO Bool
isAvailible toCheck =  do x <- readAvailible
                          if elem toCheck (dropEveryOther (splitOneOf ":'\n'" x)) 
                           then return True 
                           else return False


-- Lists all installed carrots
-- TODO - Add check to make sure the installed list exists before trying to read it!
listInstalled :: IO()
listInstalled = do result <- readInstalled
                   putStrLn result



-- See if the package is installed
isInstalled :: String -> IO Bool
isInstalled toCheck =  do check <- doesFileExist "installed.list"
                          if check 
                            then do x <- readInstalled
                                    if elem toCheck (dropEveryOther (splitOneOf ":'\n'" x)) 
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


-- Drops a given package from a list of tuples
dropPackage :: String -> [(String,String)] -> IO [(String, String)]
dropPackage package [] = return []
dropPackage package ((p,v):xs) = if package /= p
                                 then do back <- dropPackage package xs
                                         return  ((p,v):back)
                                 else do back <- dropPackage package xs
                                         return back


-- Makes a new installed.list without the removed package. This is for safety.
removePackFromFile :: String -> IO ()
removePackFromFile package = do y <- getTupleInstalled
                                clean <- (dropPackage package y)
                                home <- getHomeDirectory
                                setCurrentDirectory (home ++ "/Desktop/Rabbit") -- TODO change this to ~/Library/Preferences/Rabbit at some point
                                writeFile "tmpInstalled.list" (backToString clean)
                                renameFile "installed.list" "oldInstalled.list"
                                renameFile "tmpInstalled.list" "installed.list"
                                removeFile "oldInstalled.list"


-- Generates a string for writing to the file from a list of tuples
backToString :: [(String, String)] -> String
backToString [] = "\n"
backToString ((p,v):xs) = if (p == "") then "" else p ++ ":" ++ v ++ "\n" ++ (backToString xs)