calcDamage :: PokemonR -> PokemonR -> Move -> Maybe Int
calcDamage pA pD m = eff >>= pure . double2Int . dmg
  where
    lvl = int2Double $ getLevel pA
    pow = int2Double $ getPower m
    att = int2Double $ getAttack pA
    def = int2Double $ getDefense pD
    stab = if mType `elem` getTypes pA then 1.5 else 1
    mType = getMoveType m
    eff :: Maybe Double
    eff = product <$> traverse (lookupEff mType) (getTypes pD)
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