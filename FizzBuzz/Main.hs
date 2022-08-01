-- Ignore suggestions from Linter
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Eta reduce" #-}

module Main where

-- Start here!

import Fizz (fizzRange) -- import Calculations from Domain Layer

-- Interaction Layer: define Actions here

main :: IO ()
-- 1. Get the upper bound value from console input
-- 2. Apply `fizzRange` to the bound
-- 3. Print the results to the console
main =
--       3               2            1
    printStrings =<< fizzRange <$> getBound

getBound :: IO Int
-- 1. Prints a user prompt to console
-- 2. Gets a string input from the user
-- 3. Converts string to an integer
getBound =
--   3          2                     1
    read <$> getLine << (putStrLn "Enter upper bound: ")

--  don't worry about this: it's a custom adapter function to aid readability
    where (<<) = flip (*>)

printStrings :: [String] -> IO ()
-- 1. Transforms each string into a print action
-- 2. Executes each action, sequentially printing strings to the console
printStrings strs =
--      2              1
    executeAll (map putStrLn strs)

--  don't worry about this...
    where executeAll = foldr1 (>>)