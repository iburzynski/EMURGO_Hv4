{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant id" #-}

squareList :: [Int] -> [Int]
squareList [] = []
squareList (x:xs) = x ^ 2 : squareList xs

-- squareList [2, 3, 4] => [4, 9, 16]

negateList :: [Int] -> [Int]
negateList [] = []
negateList (x:xs) = negate x : negateList xs

-- negateList [2, 3, 4] => [-2, -3, -4]

mapInts :: (Int -> Int) -> [] Int -> [] Int
mapInts _ [] = []
mapInts f (x:xs) = f x : mapInts f xs

squareList' :: [Int] -> [Int]
squareList' = mapInts (^ 2)

negateList' :: [Int] -> [Int]
negateList' = mapInts negate

map' :: (a -> b) -> [] a -> [] b
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- fmap' :: (a -> b) -> f a -> f b

-- class Functor where
--   fmap :: (a -> b) -> f a -> f b

data Maybe' a = Nothing' | Just' a
-- data [] a =        [] | a : [] a

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

-- (<$>)
ex1 = (^ 2) <$> Just' 3
ex1' = fmap (^ 2) (Just' 3)

ex2 = (^ 2) <$> [2, 3, 4]
ex2' = fmap (^ 2) [2, 3, 4]

-- mappend / (<>)

data Shape a = Square a | Circle a | Rectangle a a
  deriving Show

instance Functor Shape where
  fmap f (Square a)      = Square (f a)
  fmap f (Circle a)      = Circle (f a)
  fmap f (Rectangle a b) = Rectangle (f a) (f b)

data Color a = Color a a a

ex3 = fmap id [1, 2, 3] -- [id 1, id 2, id 3] => [1, 2, 3]
ex3' = id [1, 2, 3] -- [1, 2, 3]

ex4 = fmap id (Just "hello") -- Just (id "hello") => Just "hello"
ex4' = id (Just "hello") -- Just "hello"