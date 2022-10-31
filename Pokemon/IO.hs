import Data.Char (toUpper)

-- Convert the `printMove` function into an IO action that prints the message to the terminal.
printMove' :: String -> String -> String
printMove' p m = map toUpper p ++ " used " ++ map toUpper m ++ "!"

printMove :: String -> String -> IO ()
printMove p m = putStrLn (map toUpper p ++ " used " ++ map toUpper m ++ "!")

printEff :: Float -> IO ()
printEff e
  | e == 2.0  = putStrLn "It's super effective!"
  | e == 0.5  = putStrLn "It's not very effective."
  | e == 0    = putStrLn "It has no effect."
  | otherwise = pure ()

