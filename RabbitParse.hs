-- Author: Cody Faust <cfaust244@gmail.com>
-- Description: More error resiliant parser for Rabbit
-- Special thanks to "Graham Hutton's - Programming in Haskell", Professor Andy Gill
-- StackOverflow, LearnYouAHaskell.com and several other websites

module RabbitParse
( Package
, readSource
) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char
import qualified Data.Map as M
import Data.Either
import Data.Maybe

-- A package is a map of name version so makes sense to have it as a type
type Package = M.Map String String


comment :: Parser ()
comment  = do string "//"
              skipMany (noneOf "\n\r")                                          -- skips everything till a newline character or a return character since it broke a test case (fun bug hunt that was >.>)
           <?> "a comment"                                                      -- "expected a comment"


endOfLine :: Parser ()
endOfLine  = do char '\n'                                                       
                return ()
             <?> "end of line"


-- Carrots are the names this determines what is a valid carrot
carrot :: Parser String
carrot = do x  <- letter <|> digit                                              -- A carrot may begin with a letter or digit only
            xs <- many (letter <|> digit <|> char '_' <|> char '-')             -- The rest may be any number of letters, digits, underscores or dashes
            return (x:xs)
         <?> "valid carrot name"                                                -- Try what is above (to the left) otherwise give us what went wrong 
                                                                                -- <?> is a Parsec operator



-- A package is a carrot and version
package :: Parser (String, String)
package = do c <- carrot
             skipMany space                                                     -- space is a function! It parses a white space char
             char '='                                                           -- This allows whitespace around the =, since in testing this was a common
             skipMany space                                                     -- cause of bugs
             v <- manyTill anyChar (try comment <|> try endOfLine <|> eof)      -- manyTill will continue for any character other than comment/endOfLine or eof 
             let stripped = reverse . dropWhile isSpace . reverse               -- Same as reverse (dropWhile isSpace (reverse v)) 
             return (c, stripped v)


-- lines are a package, a comment, or nothing, nothing will be whitespace
line :: Parser (Maybe (String, String))
line  = do skipMany space                                                       -- >> is like >>= but for when we dont return a value
           try (comment >> return Nothing) <|> (package >>= return . Just)      -- return Just package


input :: Parser [(String, String)]
input  = do contents <- many line
            return (catMaybes contents)                                         -- catMaybe takes a lit of Maybes and returns a list of all the Just values (from haskell docs)
        

-- 
readSource :: String -> IO (Either ParseError Package)
readSource filename = do results <- parseFromFile input filename                -- input is the function above, input represents the entire file
                         return $ case results of
                            Left err -> Left err
                            Right xs -> Right (createMap xs)


createMap :: [(String, String)] -> Package
createMap [] = M.empty
createMap ((p,v):xs) = M.insert p v (createMap xs)
