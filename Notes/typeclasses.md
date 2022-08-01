We can think of a typeclass as a schema describing one or more generic utility functions and/or
properties.

These utilities are called *methods*, and can augment the power of any type, provided some
implementation of the schema can be produced for it.

Typeclass methods allow us to abstract common programming patterns into

The class declaration describes some minimum set of required methods or properties for
implementation, and provides the type signatures that their implementation must satisfy.

It isn't the concern of the class how particular types will implement its requirements: it merely
provides a high-level specification.

Types are typically free to implement the requirements however they like, as long as the
implementation is compatible with the type signatures in the specification.

(Some typeclasses are based on abstractions that need to obey certain mathematical laws. In these
cases it is up to the programmer to ensure their instances are valid.)

The concrete implementation of a class's methods and/or properties for a particular type is called
an *instance*.

If a class is like an outline of requirements for membership, an instance is like a membership
application.

Each required method is like a question on that application, which must be answered by the applicant
type in its instance declaration.

The answers are just like any other definition in Haskell.

If the requirement is a property, we provide some compatible constant value.

An example is the Bounded class, which contains two required properties: minBound and maxBound.

These specify the lowest and highest possible values of a particular type.

Each of these values must be a constant value of type 'a', where 'a' is the applicant type.

If the requirement of a class is a method, we provide some compatible function definition.

For example, the Foldable class requires one of two methods to be implemented: foldr or foldMap.

Let's say we've defined a binary tree type we'd like to be able to fold over, as we learned to do
with lists.

Our type is defined as:
 ```Haskell
  data Tree a = Leaf | Node (Tree a) a (Tree a)
 ```

We can make a Foldable instance for it by defining foldr:
```Haskell
 -- class specification:
  class Foldable t where
-- A class declaration contains some type variable (here `t`), which will be replaced by some concrete type in any instance.
    foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- Tree instance implementation:
  instance Foldable Tree where
-- We replace the type variable `t` in the method's specification signature with our Tree type:
    foldr :: (a -> b -> b) -> b -> Tree a -> b
-- Note: including type signatures in instance declarations requires the {-# LANGUAGE InstanceSigs -#} pragma in GHC versions < 9.2.
    foldr _       acc Leaf                = acc
    foldr reducer acc (Node left x right) = foldr reducer (reducer x (foldr reducer acc right)) left
-- The code above is quite a mindtwister! Don't worry about understanding how it works, unless you're looking for a challenge. The details of the implementation aren't relevant for this example.
```

We declared a `Foldable` instance and defined a single method, `foldr`, which is sufficient to make
Tree a valid `Foldable` type.

We can now use `foldr` to fold any compatible reducer function over any `Tree`, just like we can
with a list.

But there doesn't seem to be any immediate  benefit to making this `Foldable` instance, apart from
the ability to call a function by a familiar name (`foldr` instead of, for example, `foldTreeR`).

We still had to write a full definition of `foldr` by hand.

Is there any other benefit from implementing class methods versus just defining regular functions?

A class often contains additional methods, which are defined entirely in terms of the required ones.

Since these bonus methods can be automatically derived from what has already been implemented, a
type will acquire them for free once it has a satisfactory instance.

Automatically gaining additional utilities by implementing a smaller subset of methods is one
reason why typeclasses are at the heart of Haskell programming.

We didn't see it happen when we completed our definition of `foldr` for `Tree`, but we can now also
use `foldl`, `foldl'`, `foldr1`, `foldl1`, `null`, `length` on `Tree` values.

We can convert a `Tree` into a list with the `toList` method.

If the data contained in a `Tree` is an instance of `Eq`, we can use the `elem` method.

If an instance of `Num`, we can use the `sum` and `product` methods.

If an instance of `Ord`, we can use the `maximum` and `minimum` methods.

And there are even more!

We just automatically gained up to 14 additional `Foldable` methods for our binary trees
(depending on the properties of the data inside them), but we only had to implement one.

