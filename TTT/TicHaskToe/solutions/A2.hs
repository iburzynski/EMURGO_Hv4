{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat [
  "Player ",
  show p,
  "'s turn: enter a row and column position (ex. A1)"
  ]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c]
  | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = E `notElem` concat b

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, X]
  , [O, X, O]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings rs = zip ['A' ..] rs

-- Q#07
formatLine :: [String] -> String
formatLine ss = concat [
  _SEP_,
  intercalate _SEP_ ss,
  _SEP_
  ]

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (i, j) = and [
    i >= 0
  , i < _SIZE_
  , j >= 0
  , j < _SIZE_
  ]

-- getSquare :: Board -> Move -> Square
-- getSquare b (i, j) = b !! i !! j

-- getSquareE :: Move -> Square
-- getSquareE = getSquare _EMPTY_BOARD_

-- getSquareT :: Move -> Square
-- getSquareT = getSquare _TIED_BOARD_

-- Q#09
-- isNonEmpty :: Board -> Move -> Bool
-- isNonEmpty b m = getSquare b m /= E

-- isNonEmptyE :: Move -> Bool
-- isNonEmptyE = isNonEmpty _EMPTY_BOARD_

-- isNonEmptyT :: Move -> Bool
-- isNonEmptyT = isNonEmpty _TIED_BOARD_

-- Q#10
stringToMove :: String -> Move
stringToMove [r, c] = (convertRowIndex r, readDigit c)
stringToMove _      = _INVALID_MOVE_

-- Q#11
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p j r = xs ++ ys'
  where
    (xs, ys) = splitAt j r
    ys'
      | null ys   = []
      | j < 0     = ys
      | otherwise = p : tail ys