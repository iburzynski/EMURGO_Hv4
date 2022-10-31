module A3 where

import A1
import A2

putSquare :: ActivePlayer -> Board -> Move -> Board
putSquare p (r : rs) (0, j) = replaceSquareInRow p j r : rs
putSquare p (r : rs) (i, j) = r : putSquare p rs (i - 1, j)
putSquare _ [] _            = []