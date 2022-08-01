Let's say we have some function that transforms some type 'a' to another type 'b'.

Perhaps we want to find the length of a username String.

We apply the length function, and it transforms "username" into an Int value.

Now we want to determine if the username is some minimum length, like ten characters.

We write another function "greater than nine" using an operator section.

If we apply this function to the result of length, it transforms the Int into a Bool value.

We can compose the two functions into a new function "isValidLength",
defined as "greater than nine dot length".

"isValidLength" will take a String as input and return a Bool value.

Composing functions like these is easy because they operate on simple values.

To produce a valid composition, we can use any compatible function whose input type matches
the type of the previous function's return value.

But what happens if our initial input data is more complex?

Imagine our username String has some additional context.

Perhaps its value comes from an unreliable source, and is potentially missing.

This is different from the String being empty: it signals some failure in our program during
transmission of the data.

We'd like to account for this failure using Haskell's Maybe context.

Our "maybe Username" value can now take one of two forms:

Nothing if the String is absent, or a String inside a Just wrapper if it's present.

The type of "maybe Username" is now "Maybe String".

We'd like to apply "is Valid Length" to "maybe Username", but we have a problem.

"is Valid Length's" input parameter has type String:

It will not accept "maybe Username" as an argument.

We can write a new version of "is Valid Length", which accepts a "Maybe String", and returns a
"Maybe Int".

It needs two patterns: one to apply if the argument is Nothing, which simply returns Nothing.

A second pattern extracts a String from a Just wrapper and applies "is Valid Length" to it.

It then applies the Just constructor to the resulting Bool value to rewrap it in the Maybe context.

Now our function accounts for whether a username value was successfully received, as well as if it
has sufficient length.

We