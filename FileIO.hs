module FileIO
( readAvailible
, readInstalled
, writeInstalled
) where

import System.Directory
import SystemLevel

-- Reads the carrots.list file and returns an IO String for whatever needs it
-- TODO: Make more generic so it works on more than one file? 
readAvailible :: IO String
readAvailible =  do tmp <- getTemporaryDirectory
                    setCurrentDirectory tmp
                    downloadMaster
                    contents <- readFile "carrots.list"
                    return contents



-- Reads the installed.list file and returns an IO String for whatever needs it
readInstalled :: IO String
readInstalled =  do home <- getHomeDirectory
                    setCurrentDirectory (home ++ "/Desktop/Rabbit")
                    contents <- readFile "installed.list"
                    return contents



writeInstalled :: String -> IO ()
writeInstalled x = do home <- getHomeDirectory
                      setCurrentDirectory (home ++ "/Desktop/Rabbit")  -- TODO change this to ~/Library/Preferences/Rabbit at some point
                      appendFile "installed.list" (x ++ "\n")