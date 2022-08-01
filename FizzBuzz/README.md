# The Onion Architecture: Building a Functional Program
Our "FizzBuzz" program consists of three components:
1. An Action to receive an upper bound value from the user
2. A Calculation that computes the FizzBuzz result for every number in the range from 1 to an upper bound
3. An Action to print the results

We can compose these into a single expression, read from right to left:
```haskell
{-
        3                       2                     1
!( printStrings ) { <== } ( fizzRange ) { <== } !( getBound )

-}
```
*Note: this is expressed as pseudo-code: not Haskell syntax!*
### Pseudocode Legend

```haskell
 {-
   ( <name> ) : Calculation
  !( <name> ) : Action
      <==     : Function application
      { }     : Adapter (built-in utility function)
 -}
```
### Adapters
* In Haskell, Actions and Calculations work with data of differing shapes, which are expressed through their *Types*

* **Type Safety:** if the shape of an output from one function doesn't match the shape of the next function's input, we can't compose them

* This protects us from type errors when we run our program...

* But we often need to sprinkle in some special utility functions into our circuitry

"The functional core is the bulk of the application.
It has all the intelligence, and the imperative shell is sort of a glue layer between the functional pieces of the system and the nasty external world of disks and networks and other things that fail and are slow."

An IO action is a "recipe" for performing IO that the runtime system must execute.