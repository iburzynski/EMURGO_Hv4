{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use concatMap" #-}

-- Define a custom variant type `Square` with 3 constructors:
  -- X, O (for filled squares) and E (for empty squares)
data Square
-- Derive an instance of the appropriate typeclass so we can check if two `Square` values are equal

-- Define another custom variant type `GameState` with 4 constructors representing the following game states:
  -- 1. The game is ongoing
  -- 2. Player X has won
  -- 3. Player O has won
  -- 4. The game is tied
data GameState


-- We can define a type synonym `ActivePlayer` to give a more semantic description of `Square` in our signatures where appropriate.
type ActivePlayer = Square

-- Define a type synonym `Row`, replacing the `()` type with the appropriate type to represent a row in the game board.
type Row = ()

-- Define a type synonym `Board`, replacing the `()` type with the appropriate type to represent the game board.
-- Use the `Row` type synonym you created above.
type Board = ()


-- Define a type synonym `Move`, replacing the `()` type with an appropriate type that contains row and column indices
type Move = ()

-- Define a function, `switchPlayer`, which takes an `ActivePlayer` value and returns the opposite value if the input is `X` or `O`.
-- Write the appropriate type signature above the function definition
-- Use pattern-matching to define separate function patterns for each possible input value (keep in mind that an `ActivePlayer` can be `E`)

switchPlayer = undefined

-- Define a function, `showSquare`, which takes a `Square` value and returns a string representation of it.
-- Write the appropriate type signature
-- Use pattern-matching to define separate function patterns for each possible input value

showSquare = undefined


-- *** Constructing an Empty Board ***
-- Rather than hard-coding the board dimensions into multiple parts of the game, we'll define the size as a constant
-- value `_SIZE_` and reference it where needed. This gives us more flexibility to change the board
-- dimensions later if we decide to make the game more challenging.
_SIZE_ :: Int
_SIZE_ = 3

-- Create another constant `_RANGE_` containing all `Int` values from 0 up to (not including) the value of `_SIZE_`.
_RANGE_ :: [Int]
_RANGE_ = undefined

-- Define a value, `emptyRow`, consisting of a number of empty squares equal to `_SIZE_`.
-- Use the built-in `replicate` function to create the row.
emptyRow :: Row
emptyRow = undefined

-- Define a value, `emptyBoard`, consisting of a number of empty rows equal to `_SIZE_`.
-- Use `replicate.
emptyBoard :: Board
emptyBoard = undefined

-- Write a function `getSquare` that takes a `Board` value and a `Move` value and returns the square at the corresponding position.
-- HINT:
  -- Use destructuring to get the row and column indices from the `Move` value
  -- Use the built-in `!!` operator with the indices to get the `Square` value

getSquare = undefined

-- Write a function `addTickToRow` that replaces the square at a given index with a square of the active player.
-- HINT:
  -- Use the built-in `splitAt` function to split the row into two parts and destructure the resulting tuple
  -- Construct a new row out of the destructured parts and the `ActivePlayer` square
replaceSquareInRow :: ActivePlayer -> Int -> Row -> Row
replaceSquareInRow = undefined


-- Define a function `getRowMoves` which takes a row index value and creates a list of all possible moves for that row.
-- You'll need to use the built-in `zip` and `repeat` functions, as well as the `_RANGE_` constant you defined above.

getRowMoves = undefined

-- Construct the list of all valid moves.
-- You'll need to use the built-in `map` and `concat` functions, as well as your `getRowMoves` function and the `_RANGE_` constant

validMoves = undefined

-- We'll define a dummy constant to represent an invalid move:
_INVALID_MOVE_ = (-1, -1)
-- Note: this isn't the only invalid move - just a default value we'll use to represent one.

-- Write a function `isValidMove` that takes `Board` and `Move` values and returns a `Bool` indicating whether the move is valid
-- HINT:
  -- First use the built-in `elem` function and the `validMoves` list you defined above
  -- Then check whether the square at the move is empty using the `getSquare` function you defined above

isValidMove = undefined

-- Write a function `isDigit` that takes a `Char` value and returns whether it is a digit.
-- HINT: Construct a range of characters from '0' to '9' and use the `elem` function

isDigit = undefined

-- Write a function `readDigit` that takes a `Char` value and converts it to the corresponding `Int` value if it is a valid digit character. Otherwise return -1.
-- HINT:
  -- Use the `isDigit` predicate and guard patterns
  -- Use the built-in `read` function to convert the character value. `read` takes a `String` value as input: think how you can convert your `Char` value to a `String` to apply `read`.
readDigit = undefined

-- Write a function `convertRowIndex` that takes a row index value (a case-insensitive letter between A and Z) and converts it to an `Int` value
-- You'll need to perform the following 3 steps:
  -- 1. Convert the `Char` input to uppercase using the built-in `toUpper` function
  -- 2. Convert the result from Step 1 to its corresponding Unicode value using the built-in `fromEnum` function
  -- 3. Subtract 65 from the result of Step 2.
convertRowIndex = undefined


-- Write a function `stringToMove` that converts a move string (i.e. "A0") into a `Move` tuple.
-- HINT:
  -- Only strings with exactly two characters should be accepted
  -- Use the functions you defined above to convert the characters to `Int` values
  -- Strings of any other length should return the `_INVALID_MOVE_` constant
stringToMove = undefined


-- HINT:
  -- The function should recurse over the rows

playMove :: ActivePlayer -> Move -> Board -> Board
playMove = undefined

-- isWinningRow :: ActivePlayer -> Row -> Bool
-- isWinningRow s = foldr reducer True
--   where
    -- reducer :: Square -> Bool -> Bool
    -- reducer s' b = b && s' == s


getGameState = undefined