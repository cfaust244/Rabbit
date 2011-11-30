module FileIO
( readAvailible
, readInstalled
) where


-- Reads the carrots.list file and returns an IO String for whatever needs it
-- TODO: Make more generic so it works on more than one file? 
readAvailible :: IO String
readAvailible =  do contents <- readFile "carrots.list"
                    return contents



-- Reads the installed.list file and returns an IO String for whatever needs it
readInstalled :: IO String
readInstalled =  do contents <- readFile "installed.list"
                    return contents