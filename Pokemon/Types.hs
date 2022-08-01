{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
import qualified Data.Map.Strict as M
import GHC.Float (int2Double, double2Int)
import Data.List (sort)

data Pokemon = Pokemon {
    getPokemonName  :: String
  , getPokedexId    :: Int
  , getLevel        :: Int
  , getMaxHP        :: Int
  , getCurHP        :: Int
  , getMaxPP        :: Int
  , getCurPP        :: Int
  , getAttack       :: Int
  , getDefense      :: Int
  , getTypes        :: [PokemonType]
  , getAbilities    :: [Ability]
  } deriving Eq

instance Ord Pokemon where
  compare p1 p2 = compare (getPokedexId p1) (getPokedexId p2)

data Ability = Ability {
    getAbilityName :: String
  , getPower :: Int
  , getAbilityPP :: Int
  , getAbilityType :: PokemonType
  } deriving Eq

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
  deriving (Show, Eq, Ord, Enum, Bounded)

calcDamage :: Pokemon -> Pokemon -> Ability -> Maybe Int
calcDamage pA pD abil = eff >>= pure . double2Int . dmg
  where
    lvl = int2Double $ getLevel pA
    pow = int2Double $ getPower abil
    att = int2Double $ getAttack pA
    def = int2Double $ getDefense pD
    stab = if getAbilityType abil `elem` getTypes pA then 1.5 else 1
    abilType = getAbilityType abil
    eff :: Maybe Double
    eff = product <$> traverse (lookupEff abilType) (getTypes pD)
    dmg e = (((2 * lvl / 5 + 2) * pow * att / def / 50) + 2) * stab * e


-- *** Generate an Effectiveness Lookup Table ***

type EffTable = M.Map PokemonType (M.Map PokemonType Double)

lookupEff :: PokemonType -> PokemonType -> Maybe Double
lookupEff aT dT = M.lookup aT effTable >>= M.lookup dT

effTable :: EffTable
effTable = M.fromList . zip types $ map (M.fromList . zip types) effs
  where
    types = [minBound .. maxBound]

effs =
  [
    [1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
  , [2.0, 1.0, 0.5, 0.5, 1.0, 2.0, 0.5, 0.0, 1.0, 1.0, 1.0, 1.0, 0.5, 2.0, 1.0]
  , [1.0, 2.0, 1.0, 1.0, 1.0, 0.5, 2.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 1.0, 1.0]
  , [1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 2.0, 0.5, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0]
  , [1.0, 1.0, 0.0, 2.0, 1.0, 2.0, 0.5, 1.0, 2.0, 1.0, 0.5, 2.0, 1.0, 1.0, 1.0]
  , [1.0, 0.5, 2.0, 1.0, 0.5, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0]
  , [1.0, 0.5, 0.5, 2.0, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0]
  , [0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0]
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 2.0, 1.0, 0.5, 0.5, 2.0, 1.0, 1.0, 2.0, 0.5]
  , [1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 2.0, 0.5, 0.5, 1.0, 1.0, 1.0, 0.5]
  , [1.0, 1.0, 0.5, 0.5, 2.0, 2.0, 0.5, 1.0, 0.5, 2.0, 0.5, 1.0, 1.0, 1.0, 0.5]
  , [1.0, 1.0, 2.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 0.5, 1.0, 1.0, 0.5]
  , [1.0, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0]
  , [1.0, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.5, 2.0, 1.0, 1.0, 0.5, 2.0]
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0]
  ]