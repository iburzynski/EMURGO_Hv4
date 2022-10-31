{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE InstanceSigs #-}

-- *** Limitations of the Functor Typeclass ***
-- The Functor typeclass's method, `fmap` (<$>), only lets us apply unary (single-parameter)
--  functions to a single value in a context.

maybeVal = Just "reverse me"
reversedMaybeVal = fmap reverse maybeVal

listVals = ["reverse me", "palindrome", "tacocat"]
reversedListVals = reverse <$> listVals

ioVal = getLine
reversedIoVal = reverse <$> ioVal

-- What if we need to apply a binary function to two arguments which are both in a context?
maybeVal1 = Just "a monad is just a monoid "
maybeVal2 = Just "in the category of endofunctors"

-- We can't use `fmap` twice like this:
-- appendMaybes = (++) <$> maybeVal1 <$> maybeVal2
-- To understand why not, let's break down what's happening:

  -- (++) :: [a] ->  [a] -> [a]
  -- or with explicit currying...
  -- (++) :: [a] -> ([a] -> [a])
  --                  ^ the return value `b` of our `fmapped` function will be a function `[a] -> [a]`
  -- fmap :: (a -> b) -> f a   -> f  b
  -- fmap    (++) ::     f [a] -> f ([a] -> [a])
  --                 Just "str" -> Just (\s -> "str" ++ s)
  -- Our append function, (++), is promoted into a functor context.
  -- It is now waiting for some argument `f [a]` (a list of `a` values, in some Functorial context)

  -- When we apply `maybeVal1` to our promoted function, what's the signature of the returned value?
  -- λ > :t (++) <$> maybeVal1
  -- λ > Maybe ([Char] -> [Char]) -> Maybe [Char] -> Maybe [Char]

  -- We can think of the result so far like this:
  -- Just (\s -> "a monad is just a monoid " ++ s) <$> Just "in the category of endofunctors"

  -- `fmap` won't work to apply our partially-applied function to its second argument...
  --  Our partially-applied function is now inside the `Maybe` context and doesn't satisfy the type signature:
  --   fmap :: (a -> b) -> f a -> f b
  --           ^ `fmap` must receive an ordinary function, not a function in a context!

-- How can we solve this type mismatch without resorting to repetitive DIY functions again?
-- Don't worry - there's an "ap" for that!
-- (<*>) :: f (a -> b) -> f a -> f b
-- Note the similarity to the ($) operator (ordinary function application):
-- ($)   ::   (a -> b) ->   a ->   b
-- This is why we call these functors "applicatives": they support application within a context

-- Ordinary function application:
--      `g     x     y     z` <- "g applied to x, y and z"
-- Applicative style (in some applicative context `f`):
-- `pure g <*> x <*> y <*> z` <- here `x`, `y`, and `z` are all values in the context `f`
-- The syntax is the same, except we need to lift `g` into the context, and use `<*>` as adapters

-- (<*>) ::   f     (a      -> b)      -> f     a      -> f     b
--            Maybe ([Char] -> [Char]) -> Maybe [Char] -> Maybe [Char]
appendMaybes    = ((++)     <$> maybeVal1) <*> maybeVal2 -- extra parens added for clarity
-- appendMaybes = pure (++) <*> maybeVal1  <*> maybeVal2

-- Note: `f <$> x` == `pure f <*> x`
-- We can use either syntax interchangeably. The version using `fmap` is more concise.

-- Recap:
-- Any time we use `fmap` to partially apply a function to some argument in a context, we receive a
--  function in that context, so we need a special "adapter" to continue applying it to arguments.

-- The Applicative "ap" operator (<*>) allows us to chain together any number of arguments in a
--   context, which means we can sequence an arbitrary number of computations using existing
--   functions instead of creating redundant custom functions.

-- An Applicative Functor isn't very different from an ordinary Functor with respect to the
--  underlying problem it helps us solve: eliminating redundancy by applying existing functions to
--  data inside an arbitrary context. Our "superfunctor" just broadens the scope of which simple
--  functions we can do this with.

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x $ min y z -- same as min x (min y z), but let's practice using ($)

readInt :: IO Int
readInt = read <$> getLine -- `read` is promoted to the IO context and applied to the String inside

minOfThreeIO :: IO Int
minOfThreeIO = minOfThree <$> readInt <*> readInt <*> readInt

-- We can keep using `<*>` to apply as many arguments as we need:
minOfFour :: (Ord a) => a -> a -> a -> a -> a
minOfFour w x y z = min w . min x $ min y z

minOfFourIO :: IO Int
minOfFourIO = pure minOfFour <*> readInt <*> readInt <*> readInt <*> readInt

-- *** Using Applicatives to Construct Values in a Context ***
-- Value constructors are just functions, so we can use `<*>` to create values of any type using
--  context-bound data:
data FullName = FullName { getFirst  :: String
                         , getMiddle :: String
                         , getLast   :: String
                         } deriving Show

-- We can construct values using record syntax, providing the values in any order like this:
-- myname = FullName { getFirst = "Ian", getLast = "Burzynski", getMiddle = "T."}

-- But even though we've defined our (product) type using record syntax, we can still construct values
--  without referencing the field names, like we would with a simple product type
--  (as long as the field values are passed to the value constructor in the correct order):
-- myName = FullName "Ian" "T." "Burzynski"

-- Using this simple value construction syntax, we can use `<*>` to apply the constructor to IO strings:
fullNameIO :: IO FullName
fullNameIO = FullName  <$> getLine <*> getLine <*> getLine
--           ^ constructor ^ getFirst  ^ getMiddle ^ getLast
--           pure FullName <*> getLine <*> getLine <*> getLine

-- Or `Maybe` strings:
fullNameMaybe :: Maybe FullName
fullNameMaybe = FullName <$> Just "Ian" <*> Just "T." <*> Just "Burzynski"

-- *** Exercise: Implement the Applicative Instance for List ***
-- Now we will create our own version of the built-in List type from scratch and make it an Applicative:
data List a = Empty | Cons a (List a)

-- Built-in List equivalents:
-- Empty == []
-- Cons 1 (Cons 2 (Cons 3 Empty)) == [1, 2, 3]

-- We will need some version of `++` (append) for our List type in our Applicative instance.
-- We can implement this by making our List a Semigroup, defining `<>` ("mappend"):
instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Empty <> xs = xs
  -- Built-in List equivalent:
  -- [] <> xs = xs
  xs <> Empty = xs
  Cons x xs <> ys = Cons x (xs <> ys)
  -- Built-in List equivalent:
  -- (x:xs) <> ys = x : xs <> ys
  --                x : x' : x'' ... : y : y' : y'' ... : []

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  -- Built-in List equivalent:
  -- fmap f (x:xs)   = (:)  (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  -- Built-in List signature:
  --      a ->   [] a
  pure x = Cons x Empty
  -- Built-in List equivalent:
  -- pure x = [x]

  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Cons f fs <*> xs = fmap f xs <> (fs <*> xs)
  --                 ^ map the first function to all values in the arguments list
  --                           ^ append that mapped list to the result of recursively `app`ing all
  --                             functions to the tail of the arguments list
  -- Example: [(+ 1), (* 2), (^ 2)] <*> [1, 2, 3]
  --          (+1) <$> [1, 2, 3] ...
  --                   [2, 3, 4] ++ ((* 2) <$> [1, 2, 3]) ...
  --                                           [2, 4, 6] ++ ((^ 2) <$> [1, 2, 3]) ...
  --                                                                   [1, 4, 9] ++ ([] <*> xs)
  --                                                                                 []
  --                   [2, 3, 4] ++ [2, 4, 6] ++ [1, 4, 9] ++ [] == [2, 3, 4, 2, 4, 6, 1, 4, 9]

--- *** Maybe Applicative ***
-- Compared to the List Applicative, the Functor and Applicative instances for the `Maybe` context
--   are much simpler. Here they are again for review:

-- instance Functor Maybe where
--   fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap _ Nothing  = Nothing
--   fmap f (Just x) = Just $ f x

-- instance Applicative Maybe where
--   pure :: a -> Maybe a
--   pure = Just

--   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--   Nothing <*> _ = Nothing
--   _ <*> _       = Nothing
--   Just f <*> Just x = pure (f x)