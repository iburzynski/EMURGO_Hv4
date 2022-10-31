{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Eta reduce" #-}
module A4 where

import A1
import A2
import A3
import Data.List (transpose)

validMoves :: [Move]
validMoves = concat $ map getRowMoves _RANGE_

isValidMove :: Board -> Move -> Bool
isValidMove b m = m `elem` filter (isNonEmpty b) validMoves

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (map show _RANGE_)

formatRow :: Row -> String
formatRow r = formatLine $ map showSquare r

formatBoard :: Board -> String
formatBoard b = unlines $ _HEADER_ : zipWith (:) ['A' ..] (map formatRow b)

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
hasWon p b = foldr (\l acc -> acc || isWinningLine p l) False $ getAllLines b

getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWon
  | hasWon O b = OWon
  | isTied b   = Tie
  | otherwise  = Playing

playMove :: ActivePlayer -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState b', b')
  where
    b' = putSquare p b m

isValidBoard :: Board -> Bool
isValidBoard rs = length rs == _SIZE_ && foldr (\r acc -> acc && length r == _SIZE_) True rs