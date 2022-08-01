-- Domain Layer
module Fizz where -- export Calculations for use in Interaction Layer

fizzRange :: Int -> [String]
-- 1. Creates a range of integers from 1 to a provided upper boundary
-- 2. Transform each integer in the range into a string using `fizz`
--                            1               2
fizzRange n = let range = [ 1 .. n ] in map fizz range

fizz :: Int -> String
--  Takes an integer and returns a corresponding string representation based on its divisibility
fizz n
--  These are "guards": a way to handle conditionals in Haskell
    | divisibleBy 15 = "FizzBuzz"
    | divisibleBy 3  = "Fizz"
    | divisibleBy 5  = "Buzz"
    | otherwise      = show n -- catch-all guard: converts the number to a string with `show`
--  Helper function
    where divisibleBy x = n `mod` x == 0