Abstraction is the essence of programming. When we write programs, we seek to avoid repetition by abstracting common patterns into generic functions. Specific functions are defined in terms of more general ones, to leverage work we've already done.

In functional programming this often takes the form of higher-order functions: powerful functions that take other functions as inputs and might produce new functions as outputs.

For example, the "map" function acts like a machine that takes some function with a signature 'a' to 'b', and produces a new function with a signature list of 'a' to list of 'b'.

[ ]

We can pass any existing function into the machine to produce a custom version tailored to operate on a list.

Without map, we'd need to define many new versions of existing functions, adapting them to take lists as their inputs, and perform recursion manually to transform all the list elements.

Instead, this recursive application has been abstracted into a more general function that can be reused again and again.

This already gives us a lot of power, but a list is just one of many contexts where Haskell data can live.

We've seen others already: a tuple, or the Maybe context we use to handle the possibility of null values.

We've even learned how to create our own data structures with custom data types.

Each of these contexts has data living inside it. Since data transformation is the essence of Haskell
programming, naturally we'd like to apply functions to that data and have it take new forms.

What if we could take the power that map gives us to an even higher level?

It would be a machine capable of producing new versions of any function, tailored to any of a wide variety
of use cases beyond just lists.

In fact, such a function exists and is called "f map". It can take any function from 'a' to 'b' and produce a new version that is able to work within any compatible context.

This generic mapping method is associated with the Functor typeclass.

We've seen how Haskell typeclasses provide interfaces with generic methods, which types can implement in their own ways.

Just like a software can have multiple implementations designed to work across particular operating system environments, a typeclass contains a set of generic methods that can be instantiated for a variety of particular types.

Once an instance is defined for a particular type, it gains the utility of one or more common, or polymorphic, methods. We've essentially implemented a version of some desired software for a new operating system, extending its versatility.

Whenever we apply a polymorphic method to a value of some supported type, we're booting up that type's particular implementation from its class instance. The method is "overloaded", which means it is called by the same name regardless of which type we call it on, but the code that runs is uniquely tied to the particular type. We can think of the Firefox browser as a single piece of software, but in actuality it exists only as particular implementations for specific operating systems.

A Functor is any data environment that supports this mapping operation: the ability to adapt existing functions to apply transformations to the data within.

Mapped functions do not affect the environment to which they are adapted. A function mapped to a list environment takes a list as input and returns a new list as output: only the elements within its structure are transformed, while the structure is preserved.


