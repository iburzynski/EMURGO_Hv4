{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >>" #-}

import Prelude hiding (log)

-- Part 3 - the Writer Monad: simulating a write-only log accumulator ***
-- Reference: https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html

-- We want some trace of our program as it runs, or to log when certain things happen.
-- We could use State, but when logging we primarily write to the global variable, not read it.

-- We can add the log as an extra return value to all our functions (inside a tuple)
type Log = [String]

addTwo :: Int -> (Log, Int)
addTwo x = (["adding 2..."], x + 2)

augmentAndStringify :: Int -> Int -> (Log, String)
augmentAndStringify x y = (newLog, show (x' + y'))
  where
    (xLog, x') = addTwo x
    (yLog, y') = addTwo y
    newLog     = concat [ ["augmenting..."]
                        , xLog
                        , yLog
                        , ["stringifying..."]
                        ]

-- Once again, this solution is awkward and not very composable.
-- Lots of surface area for errors:
--   * forgetting to use some log messages in the final output
--   * returning log messages in the wrong order
--   * duplicating log messages, etc.

-- Let's implement a new type to express the essence of a write-only accumulator:
newtype Logger a = Logger { runLogger :: (Log, a)}
-- Just like our Reader type is essentially just a generic function (a -> b),
-- Writer is essentially a generic tuple (a, b).
-- And just like we abstracted away our (global) function parameter in Reader,
-- making it part of the monadic context, we can abstract away our log value `l` and pass it around

-- Now let's refactor our functions using our new monadic type:
addTwoM :: Int -> Logger Int
addTwoM x = Logger (["adding two..."], x + 2)

augmentAndStringifyM :: Int -> Int -> Logger String
augmentAndStringifyM x y =
  Logger (["augmenting..."], ()) >>= (\_ ->
  --                         ^ a Logger with an empty return value, just to add a trace to the log
  --                                   ^ discard the useless Unit value*
    addTwoM x >>= (\x' ->
      addTwoM y >>= (\y' ->
        Logger (["stringifying..."], show (x' + y')))))

-- * note: we can also use the (>>) operator here, which is like (>>=) but discards its return value
-- see: https://www.stackage.org/haddock/lts-19.9/base-4.15.1.0/Prelude.html#v:-62--62-

-- We no longer need to manually construct the final log output or keep track of individual logs
-- By abstracting the log value away into the monadic context,
-- (>>=) now keeps track of and assembles logs for us.

instance Functor Logger where
  fmap ab (Logger (l, a)) = Logger (l, ab a)

instance Applicative Logger where
  pure a = Logger ([], a)
  --               ^ an empty Log
  Logger (l, ab) <*> Logger (l', a) = Logger (l ++ l', ab a)

instance Monad Logger where
  Logger (l, a) >>= aLb = Logger (l ++ l', b)
    where
      (l', b) = runLogger . aLb $ a

-- By abstracting over our log accumulator, we lose convenient access to the log
-- Rather than using the ugly solution with `Logger (["message..."], ()) >>= (\_ -> ...)`...
-- we can define a utility function to add to the log:
log :: String -> Logger ()
log msg = Logger ([msg], ())

-- and one to add multiple messages at once:
logs :: [String] -> Logger ()
logs msgs = Logger (msgs, ())

-- We can now refactor our arithmetic functions using `log` and do-notation:
addTwo' :: Int -> Logger Int
addTwo' x = do
  log "adding two..."
  pure $ x + 2

augmentAndStringify' :: Int -> Int -> Logger String
augmentAndStringify' x y = do
  log "augmenting..."
  x' <- addTwo' x
  y' <- addTwo' y
  log "stringifying..."
  pure . show $ x' + y'

-- We can define a more polymorphic version of Logger by allowing the log value to be any type
-- (instead of just a list of Strings).
-- But we need whichever type the log ends up as to be *combinable*, so we can append logs together.
-- Does this call to mind any Typeclass we've seen before?

-- We can define our type like this:
newtype Writer l a = Writer { runWriter :: (l, a) }
-- And put a constraint on our Applicative/Monad instances such that the log type is *combinable*

instance Functor (Writer l) where
  fmap ab (Writer la) = Writer $ ab <$> la

instance Monoid l => Applicative (Writer l) where
  pure a = Writer (mempty, a)
  Writer (l, ab) <*> Writer (l', a) = Writer (l <> l', ab a)

instance Monoid l => Monad (Writer l) where
  Writer (l, a) >>= aLb = Writer (l <> l', b)
    where
      (l', b) = runWriter . aLb $ a

-- Rewrite the `log` function for Writer:
tell :: l -> Writer l ()
tell l = Writer (l, ())

-- The `censor` utility function transforms a log value before passing it on
censor :: (l -> l) -> Writer l a -> Writer l a
censor f (Writer (l, a)) = Writer (f l, a)

-- We can read the log value using `listen`: this allows us to branch the log
listen :: Writer l a -> Writer l (a, l)
listen (Writer (l, a)) = Writer (l, (a, l))