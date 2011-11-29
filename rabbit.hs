module Main where
import System.Environment

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

help :: IO()
help = do putStrLn "install application... installs the application or applications supplied"
          putStrLn "update... updates all installed packages"
          putStrLn "remove application...removes the supplied application or applications"