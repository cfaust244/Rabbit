module Utilities
( dropEveryOther
, dropEveryOther'
) where

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