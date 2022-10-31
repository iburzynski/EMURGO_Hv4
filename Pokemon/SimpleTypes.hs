-- # Lesson 1: Values & Types

-- Key concepts:
 -- * Values vs. types
 -- * Primitive values & types
 -- * Parametric types
 -- * Function types
 -- * Function definition & application

-- Q1. Declare a `pikachu` variable, replacing `undefined` with a value containing its ID number
-- (i.e. 25) and its name.

-- Q2. Which parametric type must be chosen to define the Pokemon value, and why?

-- Q3. Which types of data will the parametric type contain?

-- Q4. What is the type of `pikachu`? Write a type signature above the variable definition.

pikachu :: (String, Int)
pikachu = ("Pikachu", 25)

-- Q5. What are two different ways you could represent the type of the name value?


-- Q6. Write a type synonym called `Pokemon` to refer to the type of your value in Q2.
type Pokemon = (String, Int)

-- Q7. Declare a list `starterPokemon` containing values for the three Pokemon below:

-- Bulbasaur (ID #1)
-- Charmander (ID #4)
-- Squirtle (ID #7)

starterPokemon :: [Pokemon]
starterPokemon = [("Bulbasaur", 1), ("Charmander", 4), ("Squirtle", 7)]

-- Q8. Write a type signature for `starterPokemon` above its definition
  -- Use the type synonym you defined in Q6.

-- Haskell has two built-in functions, `fst` and `snd`, which return the first and second elements
-- of a 2-tuple. Use these to solve the questions below.

-- Q9. Write the type signature for a function `getID`, which takes a Pokemon value as an argument
-- and returns its ID.

getID pokemon = undefined

-- Q10. Implement the `getID` function, replacing `undefined` on the right side of the equals sign.
  -- Hint: apply the `fst` function to the argument!

-- Q11. Write a second function, `getName` which returns a Pokemon's name.

-- Q12. Test `getID` and `getName` in GHCi by applying them to the `pikachu` variable defined in Q1.