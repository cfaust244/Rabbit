-- Rabbit OSX Application Manager v1.0
-- Cody Faust <cfaust244@gmail.com>

-- TODO  
-- correctly match Firefox with firefox (make it a lowercase F)


module Main where


-- Basic stuff
import RabbitParse
import Text.Parsec.Error
import SystemLevel
import qualified Data.Map as M
import System.Directory

-- Pretty terminal
import System.Console.ANSI
-- For getArgs
import System.Environment 

-- Gathers arguments, checks if its the first run, and sets the working directory
main :: IO()
main = do args <- getArgs
          home <- getHomeDirectory
          firstRun <- doesDirectoryExist (home ++ "/.rabbit")
          if firstRun 
            then setRabbitDir
            else do createDirectory (home ++ "/.rabbit")
                    setRabbitDir
          clController args



-- Decides what action to perform based on the arguments given
clController :: [String] -> IO()
clController []   = putStrLn "Enter a command please!"
clController (x:xs)
              | x == "install"  = install $ head xs
              | x == "remove"   = remove  $ head xs
              | x == "help"     = help
              | x == "search"   = search $ head xs
              | x == "list-i"   = printInstalled
              | x == "list-a"   = printAvailible
              | x == "update"   = update
              | otherwise       = putStrLn "Unknown command type help for a list of commands"



search :: String -> IO()
search package = do x <- isAvailible package
                    if x
                      then do setANSIblue
                              describe package
                              resetANSI
                      else do setANSIred
                              putStrLn "\nSorry, no packages by that name. I am a sad rabbit.\n"
                              resetANSI

-- Verifies it can install the package
-- TODO: Work on the indentation here..goes past 80 characters...
install :: String -> IO()
install package  = do ins <- isInstalled package
                      avl <- isAvailible package
                      case () of _
                                  | ins == True  -> do setANSIred
                                                       putStrLn "\nPackage is already installed!\n"
                                                       resetANSI
                                  | avl == False -> do setANSIred
                                                       putStrLn "\nPackage isn't availible!"
                                                       resetANSI
                                  | otherwise -> do p <- handleErrors $ readSource "carrots.list"
                                                    v <- getVersion package p
                                                    setANSIblue
                                                    putStrLn ("\nPackage name: " ++ package)
                                                    putStrLn ("Package version: " ++ (show v) ++ "\n")
                                                    downloadPackage package
                                                    extractAndInstallPackage package
                                                    addToSources package (show v)
                                                    putStrLn ("\n" ++ package ++ " successfully installed!\n")
                                                    resetANSI
                                                    return ()

-- Checks if the package can be removed, if so calls the removePackage function
remove :: String -> IO()
remove package = do ins <- isInstalled package
                    if ins
                      then do setANSIblue
                              putStrLn ("\nRemoving: " ++ package ++ "\n")
                              resetANSI
                              removePackage package
                      else do setANSIred
                              putStrLn "\nPackage isn't currently installed!\n"
                              resetANSI

-- Prints the help message (kinda obvious)
help :: IO ()
help = do setANSIblue 
          putStrLn "\ninstall  -> installs the requested application"
          putStrLn "remove   -> removes the requested application"
          putStrLn "update   -> updates ALL installed applications"
          putStrLn "search   -> describes the requested application if its availible"
          putStrLn "list-a   -> lists all AVAILIBLE applications"
          putStrLn "list-i   -> lists all INSTALLED applications"
          putStrLn "help     -> lists this help menu\n"
          resetANSI
           

-- Returns a boolean representing the installed state
isInstalled :: String -> IO Bool
isInstalled package = do check <- doesFileExist "installed.list"
                         case check of
                              True  -> do a <- handleErrors $ readSource "installed.list"
                                          if M.null a
                                              then return False
                                              else return $ M.member package a
                              False -> return False

-- Returns a boolean representing the availible state
isAvailible :: String -> IO Bool
isAvailible package = do downloadMaster
                         check <- doesFileExist "carrots.list"
                         case check of
                            True -> do a <- handleErrors $ readSource "carrots.list"
                                       if M.null a 
                                           then return False
                                           else return $ M.member package a
                            False -> return False
                           
-- Prints all installed applications to the screen               
printInstalled :: IO()
printInstalled = do check <- doesFileExist "installed.list"
                    if check
                      then do ins <- handleErrors $ readSource "installed.list"
                              let installed = M.toList ins
                              setANSIblue
                              putStrLn "\nInstalled Packages: \n"
                              putStrLn $ generatePrettyString installed
                              resetANSI
                      else do setANSIblue
                              putStrLn "Installed Packages: \n"
                              resetANSI


-- Prints all availible applications to the screen
printAvailible :: IO()
printAvailible = do downloadMaster
                    ins <- handleErrors $ readSource "carrots.list"
                    let aval = M.toList ins
                    setANSIblue
                    putStrLn "\nAvailible Packages: "
                    putStrLn "Remember, these are PACKAGE versions, use describe to find the software version\n"
                    putStrLn $ generatePrettyString aval
                    resetANSI


-- Creates a string representing an installed package and calls writeInstalled
addToSources :: String -> String -> IO ()
addToSources package version = writeInstalled (package ++ " = " ++ version ++ "\n")


-- Returns -1 if a problem occurs, if you get this the caller 
-- should really check it exists BEFORE
-- Asking for the version from this
getVersion :: String -> Packages -> IO Int
getVersion package ps = do let x = M.findWithDefault "-1" package ps
                           return (read x::Int) 


-- Handle any error messages generated by the parser
handleErrors :: IO (Either ParseError Packages) -> IO Packages
handleErrors xs = do x <- xs
                     case x of
                              Left x   -> do putStrLn (show x)
                                             return M.empty
                              Right x  ->    return $ x


-- Deletes the requested package and updates installed.list 
removePackage :: String -> IO()
removePackage package = do ins <- handleErrors $ readSource "installed.list"
                           let cleaned = M.toList (M.delete package ins)
                           writeFile "tmpInstalled.list" $ generateString cleaned
                           deletePackage package
                           copyFile "installed.list" "backup.list"
                           renameFile "tmpInstalled.list" "installed.list"
                           return ()




-- Updates all installed packages
update :: IO()
update = do check <- doesFileExist "installed.list"
            if check
              then do ins <- handleErrors $ readSource "installed.list"
                      downloadMaster
                      avl <- handleErrors $ readSource "carrots.list"
                      packages <- update' ins avl
                      doUpdate packages
              else return ()

update' :: Packages -> Packages -> IO [String]
update' ins avl = do let first  = M.intersection ins avl 
                     let second = M.intersection avl ins
                     --putStrLn ("Installed: " ++ (show $ M.toList first))
                     --putStrLn ("Availible: " ++ (show $ M.toList second))
                     let new = M.differenceWith diff first second
                     --putStrLn ("To Update: " ++ (show $ M.toList new))
                     let packages = M.keys new
                     if null packages 
                       then do setANSIblue
                               putStrLn "\nNothing to update!\n"
                               resetANSI
                               return []
                       else return packages
                     return packages

           
doUpdate :: [String] -> IO()
doUpdate []     = return ()
doUpdate (x:xs) = do removeOutdated x
                     installUpdates x
                     doUpdate xs                      
                     

installUpdates :: String -> IO()
installUpdates package = do setANSIblue
                            p <- handleErrors $ readSource "carrots.list"
                            v <- getVersion package p
                            putStrLn ("\nUpdating  " ++ package)
                            putStrLn ("Updated version: " ++ (show v) ++ "\n")
                            downloadPackage package
                            extractAndInstallPackage package
                            addToSources package (show v)
                            resetANSI
                            return ()


removeOutdated :: String -> IO()
removeOutdated package = do setANSIblue
                            putStrLn ("\nRemoving old: " ++ package ++ "\n")
                            resetANSI
                            removePackage package


diff :: (Eq a) => a -> a -> Maybe a 
diff iv av = if iv == av then Nothing else Just av




-- Writes the given string to installed.list
writeInstalled :: String -> IO ()
writeInstalled x = do appendFile "installed.list" x


-- Gives us a string to write back to a file
generateString :: [(String, String)] -> String
generateString [] = ""
generateString ((p,v):xs) = p ++ " = " ++ v ++ "\n" ++ (generateString xs)


-- Gives us a string formatted for printing to the screen
generatePrettyString :: [(String, String)] -> String
generatePrettyString [] = ""
generatePrettyString ((p,v):xs) = "Package: " ++  p ++ 
                                    "                   \t-> Version: " ++ 
                                    v ++ "\n" ++ (generatePrettyString xs)


setANSIblue :: IO()
setANSIblue = setSGR [ SetConsoleIntensity BoldIntensity
                     , SetColor Foreground Vivid Blue
                     ]

                     
setANSIred :: IO()
setANSIred = setSGR [ SetConsoleIntensity BoldIntensity
                    , SetColor Foreground Vivid Red
                    ]


resetANSI :: IO()
resetANSI = setSGR [Reset]