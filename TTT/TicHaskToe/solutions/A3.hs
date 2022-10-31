{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
module A3 where

import A1
import A2

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts (i:is) = show i : showInts is
showInts []     = []

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares (s:ss) = showSquare s : showSquares ss
showSquares []     = []

formatRow :: Row -> String
formatRow r = formatLine (showSquares r)

-- Q#03
formatRows :: [Row] -> [String]
formatRows (r:rs) = formatRow r : formatRows rs
formatRows []     = []

-- *** Assignment 3-2 ***

-- Q#04
prependRowIndices :: [String] -> [String]
prependRowIndices ss = go (indexRowStrings ss)
  where
    go ((c, rs):rss) = (c : rs) : go rss
    go [] = []

-- Q#05
validMoveHelper :: Row -> Int -> Bool
validMoveHelper [] _     = False
validMoveHelper (c:_)  0 = c == E
validMoveHelper (_:cs) j = validMoveHelper cs (j - 1)

-- Q#06
isValidMove :: Board -> Move -> Bool
isValidMove b m
  | isMoveInBounds m = go b m
  | otherwise        = False
  where
    go [] _          = False
    go (r:_)  (0, j) = validMoveHelper r j
    go (_:rs) (i, j) = go rs (i - 1, j)

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare p (r : rs) (0, j) = replaceSquareInRow p j r : rs
putSquare p (r : rs) (i, j) = r : putSquare p rs (i - 1, j)
putSquare _ [] _            = []