import Data.Foldable (toList, foldl')

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)
-- a Tree can contain data of any one type...
-- and is either Empty or a Node consisting of:
--   * a left branch (type Tree a)
--   * a value (type a)
--   * a right branch (type Tree a)

myTree =
  Node (Node (Node Empty 3 Empty) 1 (Node Empty 5 Empty)) 0 (Node (Node Empty 4 Empty) 2 (Node Empty 6 Empty))

--       0
--     /   \
--    1     2
--   / \   / \
--  3   5 4   6

-- *** Challenge Exercise: "Folds within Folds" ***
-- Define a Foldable instance for your binary search tree by implementing `foldr`.
instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Node left x right) =
-- 1. Make a new accumulator value by applying the reducer `f` to:
--      a.) the value of the current node `x`, and
--      b.) the new accumulator produced by folding `f` over the current acc. and the right subtree
    let newAcc = f x (foldr f acc right)
-- 2. Then fold the left subtree with `f` and the new accumulator value from Step 1.
-- These two steps alternate and repeat until the entire tree has been folded into a single value.
    in foldr f newAcc left

instance Ord a => Semigroup (Tree a) where
  Empty <> t = t
  t <> Empty = t
  tx <> ty = foldr insert tx ty

instance Ord a => Monoid (Tree a) where
  mempty = Empty

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

test = fromList [7, 2, 1, 4, 0, 19, 8]
test2 = fromList [9, 11, 5, 14]

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left y right)
  | x > y = Node left y (insert x right)
  | otherwise = Node (insert x left) y right

toList_ :: Tree a -> [a]
toList_ Empty = []
toList_ (Node left x right) = toList_ left ++ [x] ++ toList_ right

toList' :: Tree a -> [a]
toList' = foldr (:) []

elem' :: Ord a => a -> Tree a -> Bool
elem' _ Empty = False
elem' x (Node left y right)
  | x == y = True
  | x < y = elem' x left
  | x > y = elem' x right