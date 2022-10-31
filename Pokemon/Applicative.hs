import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (toUpper)
import PokeData (pokeDB, movesDB)
import Types

type Pokedex = Map Int PokeRecord
type Moves = Map Int Move


-- printMove :: String -> String -> IO ()
-- printMove p m = putStrLn (map toUpper p ++ " used " ++ map toUpper m ++ "!")

printMove :: Int -> Int -> IO ()
printMove pId mId = do
  case go <$> maybePokemon <*> maybeMove of
    Nothing -> putStrLn "Error: invalid Pokemon or move"
    Just x  -> x
  where
    go :: String -> String -> IO ()
    go p m       = putStrLn (map toUpper p ++ " used " ++ map toUpper m ++ "!")

    maybePokemon = getName <$> M.lookup pId pokeDB :: Maybe String
    maybeMove    = getMoveName <$> M.lookup mId movesDB :: Maybe String