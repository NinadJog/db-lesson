# db-lesson

Connect to a SQLite3 relational database using the `sqlite-simple` library.

The code simply follows Lesson 41 from the Manning book, _Get Programming With Haskell_ by Will Kurt. I have not done anything fundamentally new other than minor code tweaks.

I tried out the code from the book just to get familiar with doing CRUD operations in Haskell.

It contains an example of mutual recursion, where the `performCommand` and `main` functions call each other in a mutually recursive manner. Both functions are tail recursive, so the Haskell compiler ensures that they don't overflow the stack. (That's also not my innovation; it is part of the solution provided in the book.)

I made the following minor tweaks to the code.

* I used a case expression instead of guards in the `performCommand` function.

* TBD: Keep the `Main.hs` file slim; move the database operations to a separate module

