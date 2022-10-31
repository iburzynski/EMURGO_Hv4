-- # Lesson 2: Simple Functions

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Redundant lambda" #-}
type Name = String
type Level = Int
type HP = Int
type CurHP = HP
type MaxHP = HP
type Attack = Int
type Defense = Int
type Pokemon = (Name, Level, CurHP, MaxHP, Attack, Defense)

-- Key Concepts:
  -- * Pattern-matching/destructuring
  -- * Wildcard
  -- * Cons & Append
  -- * Recursion
  -- * Conditional expressions
  -- * Child scope

printMove :: String -> (String -> String)
printMove p m = p ++ " used " ++ m ++ "!"

-- CHARMANDER used FIRE BLAST!
-- CHARMANDER used EMBER!
-- CHARMANDER used SLASH!

-- PIKACHU used THUNDERBOLT!
-- PIKACHU used TAIL WHIP!
-- PIKACHU used BODY SLAM!

printMove_ :: String -> (String -> String)
printMove_ = \p -> (\m -> p ++ " used " ++ m ++ "!")

-- Exercise: Pokemon Pattern-Matching & Recursion

-- Q1. Write a function that takes a Pokemon value as an argument and returns its current HP.
  -- Hint: destructure the Pokemon tuple in the assignment portion (left side of the equals) so you
  -- can reference the values inside.
  -- Bonus: the Pokemon's other values aren't used by our function, so we don't need to
  -- assign names to them. What can we use in place of variable names for these values?
getCurHP :: Pokemon -> CurHP
getCurHP (_, _, chp, _, _, _) = chp

-- Q2. Write a function that "heals" a Pokemon.
  -- It takes a Pokemon value as an argument and returns a new Pokemon value with its CurHP value
    -- set to its MaxHP value.
  -- Is the CurHP value needed in our function? What can we use to represent it instead of assigning
    -- a variable name?
heal :: Pokemon -> Pokemon
heal (name, lvl, _, maxHP, atk, def) = (name, lvl, maxHP, maxHP, atk, def)

-- Q3. Write a recursive function that uses your `heal` function to heal an entire party
-- (list of Pokemon).
healParty :: [Pokemon] -> [Pokemon]
healParty [] = []
healParty (p:ps) = heal p : healParty ps

-- Q4. Replace the call to `heal` in Q3 with an inline lambda expression.
healParty' (p:ps) = (\(n, _, mHP) -> (n, mHP, mHP)) p : healParty' ps

-- Q5. Write a recursive function that takes an HP value and a list of Pokemon, and returns a new
-- list of Pokemon
getMinHPPokemon :: HP -> [Pokemon] -> [Pokemon]
getMinHPPokemon _ [] = []
getMinHPPokemon hp (p:ps) = if getCurHP p >= hp
  then p : getMinHPPokemon hp ps
  else getMinHPPokemon hp ps

-- ## Pokemon Quicksort

-- The order of Pokemon in your party is important! To prevent your Pokemon with less HP from being
-- overpowered, you'll want to position them so Pokemon with more HP enter the battle first.
-- Write an algorithm that sorts your Pokemon party from highest current HP to lowest.

-- For the next three question, you'll write three versions of a helper function `groupPokemon`
-- that takes three inputs:
  -- 1. An HP value
  -- 2. A Pokemon to be grouped
  -- 3. A grouping tuple containing two lists of Pokemon
-- It returns a new grouping tuple with the Pokemon prepended to one of the two lists
  -- If the Pokemon's current HP is higher than the given HP value, prepend it to the first list
  -- Otherwise, prepend it to the second list

-- Q6. Write a version `groupPokemonI` that uses an `if` expression.
groupPokemonI :: HP -> Pokemon -> ([Pokemon], [Pokemon]) -> ([Pokemon], [Pokemon])
groupPokemonI hp p (xs, ys) = if getCurHP p > hp
  then (p : xs, ys)
  else (xs, p : ys)

-- Q7. Write a version `groupPokemonC` that uses a `case` expression.
groupPokemonC :: HP -> Pokemon -> ([Pokemon], [Pokemon]) -> ([Pokemon], [Pokemon])
groupPokemonC hp p (xs, ys) = case getCurHP p > hp of
  True -> (p : xs, ys)
  False -> (xs, p : ys)

-- Q8. Write a version `groupPokemonG` that uses guard patterns.
groupPokemonG :: HP -> Pokemon -> ([Pokemon], [Pokemon]) -> ([Pokemon], [Pokemon])
groupPokemonG hp p (xs, ys)
  | getCurHP p > hp = (p : xs, ys)
  | otherwise = (xs, p : ys)

-- Q9. Next we need to define a helper function that takes an HP value and splits a list of Pokemon
-- into a grouping tuple using the `groupPokemon` function you defined above.

splitParty :: HP -> [Pokemon] -> ([Pokemon], [Pokemon])
-- Step 1. Which tuple should the function return when the party has no Pokemon?
  -- Define a base case accordingly that matches on the empty list pattern.
splitParty _ [] = ([], [])
-- Step 2.
  -- * Define a recursive case for a non-empty party (destructure the input list into head and tail).
  -- * Apply any variant of `groupPokemon` you defined above (i.e. `groupPokemonG`) to group the
  --   head Pokemon into the result of recursively calling `splitParty` on the tail Pokemon.
splitParty hp (p:ps) = groupPokemonG hp p (splitParty hp ps)

-- Q10. Define a function `sortPokemon` that sorts a party from highest current HP to lowest.
sortPokemon :: [Pokemon] -> [Pokemon]
-- Step 1. What should the function return when the input list has no Pokemon?
  -- Define a base case accordingly that matches on the empty list pattern.
sortPokemon [] = []
-- Step 2. Recursive case
  -- * Destructure the list of Pokemon into its head and tail on the left side of the equals sign.
  -- * Create a `where` block to define some intermediate variables:
    -- * `hp` = the current HP of the head Pokemon
    -- * `(xs, ys)` = the result of splitting the remaining Pokemon into a tuple of two lists using
    --   `splitParty`
  -- * At the top level of your function definition, sort `xs`, append the head Pokemon `p`, then
  --   append the result of sorting `ys`.
  -- * This will split the list recursively down into a series of ordered singleton lists, then
  --   append them all together into a new sorted list.
sortPokemon (p:ps) = sortPokemon xs ++ [p] ++ sortPokemon ys
  where
    hp = getCurHP p
    (xs, ys) = splitParty hp ps

-- Q11. Test your `sortPokemon` algorithm in GHCi on the test party below:
-- testParty :: [Pokemon]
-- testParty = [
--     ("Charmander", 50, 100)
--   , ("Pidgey", 60, 60)
--   , ("Ratata", 40, 50)
--   , ("Caterpie", 20, 40)
--   , ("Weedle", 30, 40)
--   ]