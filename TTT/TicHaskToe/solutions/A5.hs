{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

printBoard :: Board -> IO ()
printBoard b = putStrLn (formatBoard b)

_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\b -> return (getFirstPlayer b))

getMove :: Board -> IO Move
getMove b = getLine >>= go
  where
    go :: String -> IO Move
    go i
      | isValidMove b $ stringToMove i = return $ stringToMove i
      | otherwise = putStrLn "Invalid move! Try again" >> getMove b

play :: Board -> Player -> IO ()
play b p =
  when _DISPLAY_LOGO_ (putStrLn _LOGO_) >>
  printBoard b >>
  putStrLn (promptPlayer p) >>
  getMove b >>= go
  where
    go :: Move -> IO ()
    go m = case playMove p b m of
      (Playing, b') -> play b' (switchPlayer p)
      (gs, b') -> printBoard b' >> putStrLn (showGameState gs)


_LOGO_ :: String
_LOGO_ = unlines [
    "████████╗██╗ ██████╗      ██╗  ██╗ █████╗ ███████╗██╗  ██╗      ████████╗ ██████╗ ███████╗"
  , "╚══██╔══╝██║██╔════╝      ██║  ██║██╔══██╗██╔════╝██║ ██╔╝      ╚══██╔══╝██╔═══██╗██╔════╝"
  , "   ██║   ██║██║     █████╗███████║███████║███████╗█████╔╝ █████╗   ██║   ██║   ██║█████╗"
  , "   ██║   ██║██║     ╚════╝██╔══██║██╔══██║╚════██║██╔═██╗ ╚════╝   ██║   ██║   ██║██╔══╝ "
  , "   ██║   ██║╚██████╗      ██║  ██║██║  ██║███████║██║  ██╗         ██║   ╚██████╔╝███████╗ "
  , "   ╚═╝   ╚═╝ ╚═════╝      ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝         ╚═╝    ╚═════╝ ╚══════╝ "
  ]