{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
type Name = String
type MaxHP = Int
type CurHP = Int
type Pokemon = (Name, CurHP, MaxHP)

heal :: Pokemon -> Pokemon
heal (name, _, maxHP) = (name, maxHP, maxHP)

healParty :: [Pokemon] -> [Pokemon]
healParty [] = []
healParty (p:ps) = heal p : healParty ps

-- Exercise: write an algorithm that sorts your Pokemon party from highest current HP to lowest.

splitParty :: [Pokemon] -> ([Pokemon], [Pokemon])
splitParty = undefined

select :: Pokemon -> Pokemon -> ([Pokemon], [Pokemon]) -> ([Pokemon], [Pokemon])
select p1 p2 (xs, ys)
  | p2 < p1 = (p2 : xs, ys)
  | otherwise = (xs, p2 : ys)