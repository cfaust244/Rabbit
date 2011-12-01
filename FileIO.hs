module FileIO
( readAvailible
, readInstalled
) where

import System.Directory
import SystemLevel

-- Reads the carrots.list file and returns an IO String for whatever needs it
-- TODO: Make more generic so it works on more than one file? 
readAvailible :: IO String
readAvailible =  do downloadMaster
                    setCurrentDirectory "/private/var/tmp/"
                    contents <- readFile "carrots.list"
                    return contents



-- Reads the installed.list file and returns an IO String for whatever needs it
readInstalled :: IO String
readInstalled =  do contents <- readFile "installed.list"
                    return contents