{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant lambda" #-}

-- *** The State Monad: Simulating global variables using pure functions ***
-- Reference: https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
-- Note: the syntax from the tutorial above has been modified for greater clarity and conformity
--   with the built-in State type.

-- Naive solution for simulating state: pass the state "variable" as a normal parameter/return value
-- In this example our "state" will count the number of function calls
type Count = Int

reverseWithCount :: [a] -> Count -> ([a], Count)
-- To better understand the example, let's write this function with explicit currying:
--   instead of a function with 2 arguments (a list and a count), we'll view it as a function that
--   takes a list as argument and returns a lambda function `Count -> ([a], Count)`.
reverseWithCount list = \count -> (reverse list, count + 1)

-- When we compose multiple stateful computations together, we need to manually apply each of them
--   within a `let` block, so we can destructure their return values and "thread" the state through:
appendReversedWithCount :: [a] -> [a] -> Count -> ([a], Count)
appendReversedWithCount list1 list2 = \count1 -> let
  (revList1, count2) = reverseWithCount list1 count1
  (revList2, count3) = reverseWithCount list2 count2
  in (revList1 ++ revList2, count3 + 1)

-- As the number of stateful computations grows, this process becomes more tedious:
append3ReversedWithCount :: [a] -> [a] -> [a] -> Count -> (Count, [a])
append3ReversedWithCount list1 list2 list3 = \count -> let
  (revList1, count1) = reverseWithCount list1 count
  (revList2, count2) = reverseWithCount list2 count
  (revList3, count3) = reverseWithCount list3 count
  in (count3 + 1, concat [ revList1
                         , revList2
                         , revList3 ])

-- This solution requires lots of boilerplate code to thread the "state" through
-- It also increases the surface area where mistakes can occur

-- Monads allow us to have functions that manage extra "context" for us such as state
-- We can abstract away the state management concern by building it into the context
-- Then we don't need redundant code to manually thread the "state" value:
-- the bind function (>>=) will handle this threading for us

-- *** The State Type ***
-- Each of the functions above can be understood as taking some number of ordinary input arguments,
--   and returning a "state handler" function with type `state -> (state, a)`.
--   (We have explicitly curried these functions to make this interpretation easier to see.)
-- A state handler takes in a current state and returns an updated one along with its primary return
--   value.

-- We can use this to construct a type by simply wrapping it in a `newtype`:
newtype State s a = State { runState :: s -> (a, s) }
--                  ^ type wrapper
--                          ^ getter function
--                                      ^ state handler

-- This type definition is a little strange: we're used to defining types that contain primitive
--   values, not functions. But remember, functions are first-class values:
--   a function of type `a -> b` is as valid a piece of data for another type to contain as a value
--   of type `a` or `b`.

-- Our new State type is nothing other than a type wrapper (i.e. value constructor) containing a
--   state handler function. We can pass State values around and run them in our code to perform
--   stateful operations, as long as we unwrap the handler and supply it a valid state as argument.

-- *** Refactoring with State ***
-- We'll pretend the Functor, Applicative, and Monad instances already exist, and refactor our code:
reverseWithCountM :: [a] -> State Count [a]
reverseWithCountM list = State (\count -> (reverse list, count + 1))

-- (>>=) :: m a -> (a -> m b) -> m b
appendReversedWithCountM :: [a] -> [a] -> State Count [a]
appendReversedWithCountM list1 list2 =
  reverseWithCountM list1 >>= (\revList1 ->
    --                          ^ each primary return value that we were extracting from the return
    --                            tuple using pattern-matching now becomes the input parameter to
    --                            the next monadic computation
    reverseWithCountM list2 >>= (\revList2 ->
      State (\count -> (revList1 ++ revList2, count + 1))))
-- The `count` variable is now being updated and threaded through the nested function calls...
--   but it is abstracted away so we don't need to explicitly manage it until the final step.

-- It's as if it has totally disappeared - but if we look at how (>>=) is implemented for our State
--   type, it will be clear that this isn't magic; it's been built into the behavior of (>>=):
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State sas >>= aSsbs = State (\s1 ->
--                      ^ (>>=) returns a value of type `State s b`, so we start constructing one:
--                        We start with the `State` value constructor, then initialize the internal
--                        state handler by creating a lambda with input parameter `s` (:: s).
    let
      (a, s2) = sas s1
--    ^ This looks familiar! We're threading our initial state `s1` into the first stateful
--      computation, and destructuring its result to get a value `a` and an updated state `s2`.
--
--    Compare with this:
--    (revList1, count1) = (reverseWithCount list1) count
--                         ^ this produces a state handler function similar to `sas`
--                                                  ^ we then pass it some state value `s` to run it
    in
--    Now we need to produce a return value for the state handler with type `(b, s)`.
--    This will require some sophisticated "type Tetris", but it follows logically from the types of
--      the values that are available to us:
--      * a  (:: a)
--      * s2 (:: s) Note: the current state has been updated, so we now use `s2` instead of `s1`
--      * aSsbs (:: a -> State s b; i.e. a -> State (s -> (b, s)))
      runState (aSsbs a) s2)
--              ^ applying `aSsbs` to `a` gives us a value `State (s -> (b, s))`
--    ^ applying the getter function unwraps the state handler (:: s -> (b, s))
--                       ^ applying the handler to current state `s2` gives us the `(b, s)` we need

-- The Functor and Applicative instances are similar, but less complex:
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap ab (State sas) = State (\s1 ->
    let
      (a, s2) = sas s1
--              ^ to get the `a` value we need to pass to `ab`, we have to run the state handler,
--                thus the state gets updated in the process
    in (ab a, s2))
--      ^ then we just transform the `a` value into a `b` and return it along with the new state

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s1 -> (a, s1))
  --                      ^ promoting a value `a` into the State context just packages it into an
  --                        "identity" state handler that leaves the state unmodified

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State sabs <*> State sas = State (\s1 ->
    let
  --  We now need to run two state handlers to extract the function `ab` and its argument `a`.
  --  The state is threaded through each of them and is updated twice:
      (ab, s2)       = sabs s1
      (a,  s3)       = sas  s2
    in (ab a, s3))

-- *** State Utilities ***
-- Now that we've abstracted away our state management, how can we inspect and modify the state?
-- We need a few utilities to handle this.
-- Each of these produces a new State value that can be inserted into a sequence of stateful
--   computations to interact with the background process of managing state.

-- Retrieve current state:
-- The only portion of our state handler that we can interact with directly is its primary return
--   value, `a`. This is what's exposed when we compose State computations via (>>=).
get :: State s s
get = State (\s -> (s, s))
-- So to retrieve the current state, we need to duplicate it in both values of the returned tuple

-- Replace current state with a given value:
put :: s -> State s ()
put s' = State (\_ -> ((), s'))
--               ^ we are throwing away the current state (i.e. `s`), so we don't need to name it
--                     ^ we aren't returning any meaningful primary value here: just performing the
--                       effect of updating the state. So like IO Actions that only perform effects,
--                       we return the empty Unit type `()`.
--                         ^ the provided new state `s'` is simply inserted into the state position
-- Alternative implementation: `State $ const ((), s)`

-- Apply a function to update the current state:
modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))
--                           ^ similar to `put`, but updates the state by applying a transformation
--                             to it instead of just replacing it with some value

-- Alternatively, using do-notation:
modify' :: (s -> s) -> State s ()
modify' f = do -- creating a `do` block enters the State context
  currState <- get -- `get` produces a State value, containing a handler that duplicates its state
  --                     into the primary position. From there we can bind it via (<-)
  put (f currState) -- put produces a value of type `State s ()`, matching `modify'` return type

  -- Here's what's really happening behind the "magic":
  --   State (\s -> (s, s)) >>=
    --               ^ this `s` value will bind to `currState` in the next lambda
  --       (\currState -> State (\_ -> ((), f currState)))

-- We can now refactor the original functions using `do` notation and the State utility functions:
reverseWithCount' :: [a] -> State Count [a]
reverseWithCount' list = do -- enter the State context with `do`
  modify (+ 1) -- manually increment count to account for the upcoming `reverse` operation
  pure (reverse list) -- reverse the input list and lift it into a State value

appendReversedWithCount' :: [a] -> [a] -> State Count [a]
appendReversedWithCount' list1 list2 = do -- enter the State context with `do`
  revList1 <- reverseWithCount' list1 -- count is incremented in the background
  revList2 <- reverseWithCount' list2 -- count is incremented again in the background
  modify (+ 1) -- manually increment count to account for the upcoming append operation
  pure (revList1 ++ revList2) -- append the lists and lift them into a State value

append3ReversedWithCount' :: [a] -> [a] -> [a] -> State Count [a]
append3ReversedWithCount' list1 list2 list3 = do
  revList1 <- reverseWithCount' list1
  revList2 <- reverseWithCount' list2
  revList3 <- reverseWithCount' list3
  modify (+ 1)
  pure (concat [revList1, revList2, revList3])

-- *** Running a Stateful Computation ***
-- What we've produced so far can be understood as "recipes" that describe sequences of stateful
--   computation in a declarative way.
-- But how do we actualize these recipes and actually run them?
ex1 = runState (appendReversedWithCountM "tacocat" "racecar") 0
--                                                            ^ initial state argument for handler
--              ^ returns a State value
--    ^ extracts the state handler
--
ex2 = runState (append3ReversedWithCount' "part" "evil" "star") 0