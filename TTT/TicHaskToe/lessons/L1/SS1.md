# **Self-Study 1: Reading Haskell Documentation**

Visit [hoogle.haskell.org](https://hoogle.haskell.org). We recommend bookmarking this page somewhere easy to access, like your browser toolbar, as you'll need to reference it often.

Use Hoogle to look up the following list functions and practice using them in a GHCi REPL session:

* `(:)` & `(++)`:
  >Called "cons" and "append", respectively
  >These functions are infix operators: they are usually inserted between their arguments like other operators we've seen (`+`, `-`, `&&`, `||`):
  ```shell
  GHCi> 1 : [2, 3]
  [1, 2, 3]
  GHCi> 1 : []
  [1]
  GHCi> "I choose you, " ++ "Pikachu!"
  "I choose you, Pikachu!"
  GHCi>
  ```
  >Like all operators, they can also be used in prefix notation by enclosing them in parentheses:
  ```shell
  GHCi> (++) "I choose you, " "Pikachu!"
  "I choose you, Pikachu!"
  ```
  >Pay careful attention to the signatures of these functions:
  ```haskell
  (:)  ::  a  -> [a] -> [a]
  (++) :: [a] -> [a] -> [a]
  ```
  >"cons" takes a value of any type `a` and a list of other `a` values, and prepends the value to the list.
  >"append" takes two lists containing values of any type `a` and appends the second list to the first.
  >New Haskellers frequently confuse the use of these functions and encounter type errors. How can we add a single element to the end of a list? Note that we can convert any regular value into a list (called a "singleton") by enclosing it in square brackets.
* `concat`
  >Notice `concat`'s signature:
  ```haskell
  concat :: Foldable t => t [a] -> [a]
  ```
  >What this means is that `concat` can take any collection `t` containing lists of `a` values, and concatenate the contents into a list of `a` values - provided the collection is an instance of the `Foldable` class.
  >For our purposes, the only `Foldable` collection we'll be using with `concat` is a list of lists, so we can understand the signature more clearly as this:
  ```haskell
  concat :: [[a]] -> [a]
  ```
  >In other words, `concat` takes a nested list (a list of lists of `a` values) and flattens it into a single list of `a` values.
* `null`
* `head`, `tail`, `last`, `init`
  >**Note:** these functions throw runtime exceptions when called on empty lists, so they must be used cautiously.
* `elem` & `notElem`
  >As with `concat`, these functions work on various `Foldable` collections - not just lists - but we'll be using them with lists exclusively. Consider the simplified signatures below:
  ```haskell
  elem    :: Eq a => a -> [a] -> Bool
  notElem :: Eq a => a -> [a] -> Bool
  ```
  >In practice, these functions are typically used with **infix** notation for readability, like the various binary operators we've encountered (`+`, `-`, `&&`, `||`). We can make any binary function infix by enclosing it in backticks and inserting it between its two arguments:
  ```shell
  GHCi> 'x' `elem` "Snorlax"
  True
  ```
* `replicate`
  >This function is surprisingly useful. We'll use it to construct an empty board in our game.
* `splitAt`
  >Try using this function with negative and out-of-range index values. What happens?
* `intercalate`
  >This function isn't part of Haskell's Prelude, so you'll need to import the `Data.List` module in your REPL session as follows:
    ```shell
    GHCi> :m Data.List
    GHCi Data.List>
    ```
  >You can now run this command to verify that the function is available:
    ```shell
    GHCi Data.List> :t intercalate
    intercalate :: [a] -> [[a]] -> [a]
    ```
* `and`

There are many other helpful list functions available in Haskell's Prelude and the `Data.List` module. We'll use only some of them in this course, but we encourage you to explore the documentation for `Data.List` on Hoogle and familiarize yourself with its functions for future reference.