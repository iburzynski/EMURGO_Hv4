{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}
module Types where

import qualified Data.Map.Strict as M
import GHC.Float (int2Double, double2Int)
import Data.List (sort)


type Name = String
type Level = Int
type HP = Int
type CurHP = HP
type MaxHP = HP
type Attack = Int
type Defense = Int
type Special = Int
type Height = Float
type Weight = Float
type Species = String
type Desc = String

data PokeData = PokeData Int Name Species Height Weight Desc
  deriving Eq

bulbasaurData = PokeRecord 1 "Bulbasaur" "Seed" 0.7 6.9 "A strange seed was planted on its back at birth. The plant sprouts and grows with this Pokémon."

data PokeRecord = PokeRecord {
    getId      :: Int
  , getName    :: String
  , getSpecies :: String
  , getHeight  :: Float
  , getWeight  :: Float
  , getDesc    :: String
}

-- Define a `Show` instance for `PokeRecord` so a

instance Show PokeRecord where
  show :: PokeRecord -> String
  show p = concat [
      getName p
    , "\n"
    , getSpecies p
    , " Pokémon\n"
    , "Height: "
    , show (getHeight p)
    , " m\n"
    , "Weight: "
    , show (getWeight p)
    , " kg\n"
    , getDesc p
    ]

bulbasaurRec = PokeRecord {
    getId = 1
  , getName = "Bulbasaur"
  , getSpecies = "Seed"
  , getHeight = 0.7
  , getWeight = 6.9
  , getDesc = "A strange seed was planted on its back at birth. The plant sprouts and grows with this Pokémon."
  }

charmanderRec = PokeRecord {
    getId = 4
  , getName = "Charmander"
  , getSpecies = "Lizard"
  , getHeight = 0.6
  , getWeight = 8.5
  , getDesc = "Obviously prefers hot places. When it rains, steam is said to spout from the tip of its tail."
  }

squirtleRec = PokeRecord {
    getId = 7
  , getName = "Squirtle"
  , getSpecies = "Tiny Turtle"
  , getHeight = 0.5
  , getWeight = 9.0
  , getDesc = "After birth, its back swells and hardens into a shell. Powerfully sprays foam from its mouth."
  }
-- Change the `Pokemon` type synonym into a product type using the `data` keyword.
-- Use `PokeRecord` instead of `Name` as the type of the first field.

-- type Pokemon = (Name, Level, CurHP, MaxHP, Attack, Defense)
data Pokemon = Pokemon PokeData Level CurHP MaxHP Attack Defense

-- Construct a Pokemon value by applying the `Pokemon` value constructor to the required field arguments.
-- myPokemon :: Pokemon
-- myPokemon = Pokemon bulbasaurData 1 45 45 49 49

-- Define a sum type called `PokemonType` with value constructors for the following Pokemon types:
  -- Normal, Fight, Flying, Poison, Ground, Rock, Bug, Ghost, Fire, Water, Grass, Electric, Psychic,
  -- Ice, Dragon
data PokemonType =
    Normal
  | Fight
  | Flying
  | Poison
  | Ground
  | Rock
  | Bug
  | Ghost
  | Fire
  | Water
  | Grass
  | Electric
  | Psychic
  | Ice
  | Dragon
-- Derive the following instances for `PokemonType`:
  -- * Show
  -- * Enum
  -- * Bounded
  deriving (Show, Enum, Bounded)

-- The `Enum` class allows us to enumerate values of our type. It assigns a unique `Int` value to each value constructor, beginning with 0.
-- An `Enum` instance makes it possible to create a range of values using `..` (i.e. [1 .. 10]).
-- A `Bounded` instance specifies a `minBound` and `maxBound` value for a type, which can be used as boundaries for a range.

-- Construct a list of all possible Pokemon types in the same order in which you defined the value constructors.
-- Use range syntax with `minBound` and `maxBound`.
allTypes :: [PokemonType]
allTypes = [minBound .. maxBound]


data PokemonR = PokemonR {
    getPokedex      :: PokeData
  , getLevel        :: Level
  , getMaxHP        :: CurHP
  , getCurHP        :: MaxHP
  , getAttack       :: Attack
  , getDefense      :: Defense
  , getSpecial      :: Special
  , getTypes        :: [PokemonType]
  , getMoves        :: [Move]
  , getStatus       :: Status
  }

-- Refactor `heal` using the "getter" and "setter" functions provided by record syntax
heal :: PokemonR -> PokemonR
heal p = p { getCurHP = getMaxHP p }

-- instance Ord Pokemon where
  -- compare p1 p2 = compare (getPokedexId p1) (getPokedexId p2)

data Move = Move {
    getMoveName :: String
  , getPower    :: Int
  , getMoveType :: PokemonType
  }

tackle = Move "Tackle" 35 Normal

data Tree a = Empty | Node (Tree a) a (Tree a)
type Pokedex = Tree PokeRecord

data Duration = Finite Int | Infinite

data StatusKind = NonVolatile | Volatile Duration | Battle Duration

data Status = NoStatus | Status { getEffect :: StatusEffect }

data StatusEffect = FNT | BRN | FRZ | PAR | PSN | SLP Int
  deriving (Show, Eq)


setStatusEffect :: PokemonR -> StatusEffect -> PokemonR
setStatusEffect p se = case getStatus p of
  NoStatus -> p { getStatus = Status se }
  _        -> p

damage :: PokemonR -> Int -> PokemonR
damage p d = p { getCurHP  = newHP
               , getStatus = if newHP == 0 then Status FNT else getStatus p
               }
  where
    newHP = max 0 (getCurHP p - d)

poison :: PokemonR -> PokemonR
poison p
  | getEffect (getStatus p) == PSN = damage p $ getMaxHP p `div` 16
  | otherwise                      = setStatusEffect p PSN

burn :: PokemonR -> PokemonR
burn p
  | getEffect (getStatus p) == BRN = damage p $ getMaxHP p `div` 8
  | otherwise                      = setStatusEffect p BRN