{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Char (toUpper)
{-# HLINT ignore "Use map" #-}
type Name = String
type Level = Int
type HP = Int
type CurHP = HP
type MaxHP = HP
type Attack = Int
type Defense = Int
type Pokemon = (Name, Level, CurHP, MaxHP, Attack, Defense)

-- For this lesson, we'll reuse several functions we defined in the previous lesson:
getCurHP :: Pokemon -> CurHP
getCurHP (_, _, hp, _, _, _) = hp

heal :: Pokemon -> Pokemon
heal (name, lvl, _, maxHP, atk, def) = (name, lvl, maxHP, maxHP, atk, def)

groupPokemon :: HP -> Pokemon -> ([Pokemon], [Pokemon]) -> ([Pokemon], [Pokemon])
groupPokemon hp p (xs, ys)
  | getCurHP p > hp = (p : xs, ys)
  | otherwise = (xs, p : ys)

sortPokemon :: [Pokemon] -> [Pokemon]
sortPokemon [] = []
sortPokemon (p:ps) = sortPokemon xs ++ [p] ++ sortPokemon ys
  where
    hp = getCurHP p
    (xs, ys) = splitParty hp ps


-- Q1. Refactor the `healParty` function to use `map`.

healParty' :: [Pokemon] -> [Pokemon]
healParty' [] = []
healParty' (p:ps) = heal p : healParty' ps

healParty :: [Pokemon] -> [Pokemon]
healParty = map heal

-- Update the `printMove` function to display the Pokemon name and move name as uppercased.
  -- Use `map` and the built-in `toUpper` function imported from `Data.Char`.
printMove :: String -> String -> String
printMove p m = map toUpper p ++ " used " ++ map toUpper m ++ "!"

-- Q4. Refactor the `getMinHPPokemon` function to use `filter`.
  -- Write the predicate argument for `filter` using a lambda expression.

getMinHPPokemon' :: HP -> [Pokemon] -> [Pokemon]
getMinHPPokemon' _ [] = []
getMinHPPokemon' hp (p:ps) = if getCurHP p >= hp
  then p : getMinHPPokemon' hp ps
  else getMinHPPokemon' hp ps

getMinHPPokemon :: HP -> [Pokemon] -> [Pokemon]
getMinHPPokemon hp = filter (\p -> getCurHP p >= hp)

-- Q5. Write a predicate function `has50PercentHP` that returns whether a Pokemon has at least 50%
-- of its maximum HP.
  -- Hint: use the built-in `fromIntegral` function to convert Int values to floating point numbers.
has50PercentHP :: Pokemon -> Bool
has50PercentHP (_, _, cHP, mHP, _, _) = fromIntegral cHP / fromIntegral mHP >= 0.5

-- Q6. Use your predicate above to write a function that returns only Pokemon from a party with 50%+
-- of their max HP.
get50PercentHPPokemon :: [Pokemon] -> [Pokemon]
get50PercentHPPokemon = filter has50PercentHP

-- Q7. Refactor the `splitParty` function from the previous lesson using `foldr`.
splitParty' :: HP -> [Pokemon] -> ([Pokemon], [Pokemon])
splitParty' _ [] = ([], [])
splitParty' hp (p:ps) = groupPokemon hp p (splitParty' hp ps)

splitParty :: HP -> [Pokemon] -> ([Pokemon], [Pokemon])
splitParty hp = foldr (groupPokemon hp) ([], [])

-- Q8. Refactor the lambda predicate function for filter.
  -- Use the compose operator (.) and an operator section.
getMinHPPokemonR :: HP -> [Pokemon] -> [Pokemon]
getMinHPPokemonR hp = filter ((>= hp) . getCurHP)

-- Q9. Define a variable `newParty` and assign its value equal to `testParty` with the following
-- transformations applied using apply ($) and compose (.):
  -- 1. Heal the party
  -- 2. Remove Pokemon with current HP less than 50
  -- 3. Sort the Pokemon from highest current HP to lowest

-- testParty :: [Pokemon]
-- testParty = [
--     ("Charmander", 5, 50, 100)
--   , ("Ratata", 3, 40, 50)
--   , ("Pidgey", 4, 60, 60)
--   , ("Caterpie", 2, 20, 40)
--   , ("Weedle", 3, 30, 40)
--   ]

-- newParty = sortPokemon . getMinHPPokemon 50 . healParty $ testParty