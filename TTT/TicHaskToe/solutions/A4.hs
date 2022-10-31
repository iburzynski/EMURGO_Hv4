{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use any" #-}
module A4 where

import A1
import A2
import A3 hiding (_HEADER_, formatRow, formatRows, prependRowIndices, getDiagonals)
import Data.List (transpose)

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = ' ' : formatLine (map show _RANGE_)

formatRow :: Row -> String
formatRow r = formatLine $ map showSquare r

formatRows :: [Row] -> [String]
formatRows rs = map formatRow rs

-- Q#02
dropFirstCol :: Board -> Board
dropFirstCol b = map tail b

dropLastCol :: Board -> Board
dropLastCol b = map init b

-- Q#03
getDiag1 :: Board -> Line
getDiag1 []       = []
getDiag1 (r : rs) = head r : getDiag1 (dropFirstCol rs)

getDiag2 :: Board -> Line
getDiag2 []       = []
getDiag2 (r : rs) = last r : getDiag2 (dropLastCol rs)

getAllLines :: Board -> [Line]
getAllLines b = concat [b, transpose b, [getDiag1 b, getDiag2 b]]

-- Q#04
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ p l = null $ filter (/= p) l

hasWon_ :: Player -> Board -> Bool
hasWon_ p b = not . null $ filter (isWinningLine p) (getAllLines b)

-- *** Assignment 4-2 *** --

-- Q#05
isWinningLine :: Player -> Line -> Bool
isWinningLine p l = foldr (\s acc -> acc && s == p) True l

hasWon :: Player -> Board -> Bool
hasWon p b = foldr (\l acc -> acc || isWinningLine p l) False $ getAllLines b

-- Q#06
getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWon
  | hasWon O b = OWon
  | isTied b   = Tie
  | otherwise  = Playing

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState b', b')
  where
    b' = putSquare p b m

-- Q#07
prependRowIndices :: [String] -> [String]
prependRowIndices ss = zipWith (:) ['A' .. ] ss

-- Q#08
formatBoard :: Board -> String
formatBoard b = unlines . (_HEADER_ :) . prependRowIndices $ formatRows b