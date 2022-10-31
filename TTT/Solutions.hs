{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use !!" #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.Char (toUpper)
import Data.List (transpose)

data Square = X | O | E
  deriving (Eq, Show)

data GameState = Playing | XWon | OWon | Tie
  deriving Eq

type ActivePlayer = Square

type Row  = [Square]
type Line = [Square]

type Board = [Row]

type Move = (Int, Int)

_SIZE_ :: Int
_SIZE_ = 3

convertRowIndex :: Char -> Int
convertRowIndex i = fromEnum (toUpper i) - 65

-- *** Lists *** --

_RANGE_ :: [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

emptyRow :: Row
emptyRow = replicate _SIZE_ E

emptyBoard :: Board
emptyBoard = replicate _SIZE_ emptyRow

getSquare :: Board -> Move -> Square
getSquare b (i, j) = b !! i !! j

replaceSquareInRow :: ActivePlayer -> Int -> Row -> Row
replaceSquareInRow p j r = xs ++ (p : ys)
  where
    (xs, _:ys) = splitAt j r

getRowMoves :: Int -> [Move]
getRowMoves i = zip (repeat i) _RANGE_

isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c]
  | otherwise = -1

isTied :: Board -> Bool
isTied b = E `notElem` concat b

stringToMove :: String -> Move
stringToMove [r, c] = (convertRowIndex r, readDigit c)
stringToMove _      = _INVALID_MOVE_

_INVALID_MOVE_ :: Move
_INVALID_MOVE_ = (-1, -1)

-- *** Recursion *** --

playMove :: ActivePlayer -> Move -> Board -> Board
playMove p (0, j) (r : rs) = replaceSquareInRow p j r : rs
playMove p (i, j) (r : rs) = r : playMove p (i - 1, j) rs
playMove _ _ [] = []

-- *** Higher-Order Functions *** --

validMoves :: [Move]
validMoves = concat $ map getRowMoves _RANGE_

isValidMove :: Board -> Move -> Bool
isValidMove b m = m `elem` validMoves && getSquare b m == E

getDiagonals :: Board -> [Line]
getDiagonals b = [d1, d2]
  where
    maxColIndex = length b - 1
    d1 = foldr (\r acc -> r !! length acc : acc) [] b
    d2 = foldr (\r acc -> r !! (maxColIndex - length acc) : acc) [] b

getAllLines :: Board -> [Line]
getAllLines b = concat [b, transpose b, getDiagonals b]

isWinningLine :: ActivePlayer -> Line -> Bool
isWinningLine p l = foldr (\s acc -> acc && s == p) True l

hasWon :: ActivePlayer -> Board -> Bool
hasWon p b = foldr (\r acc -> acc || isWinningLine p r) False b

getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWon
  | hasWon O b = OWon
  | isTied b   = Tie
  | otherwise  = Playing


isValidBoard :: Board -> Bool
isValidBoard rs = length rs == _SIZE_ && foldr (\r acc -> acc && length r == _SIZE_) True rs

testBoard = [
  [X, E, O],
  [E, E, E],
  [O, E, X]]