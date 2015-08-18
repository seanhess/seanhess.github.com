---
layout: post
title: Practical Haskell - Using Monads
---

{{ page.title }}
================

This is part of a tutorial series intended to introduce Haskell by coding things that work. In this article we will teach you how to use Monads without panicking.

1. [Getting Started][getting-started]
2. [Importing Code][importing-code]
3. [**Using Monads**][using-monads]
4. [Build a JSON API][json-api]

Before you Begin
----------------

This article assumes a basic understanding of Haskell Types. You should know how to add a type declaration to a simple function, and understand the difference between `String`, `Maybe String`, and `[String]`. Consider reading [Chapter 8 of Learn You a Haskell: Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses). You'll probably need to have read Chapters 2 and 3 first.

Please work through [Getting Started][getting-started] so you know how to run code.

Don't Panic
-----------

Monads are not scary. You've been using them since the beginning of this tutorial. Whenever we make an `IO` function, and use `do`, we're using monads. Let's take a look at a simple example:

    main = do
      name <- getLine
      putStrLn ("Hello, " ++ name)

You probably have a pretty good feel for what this is doing. Trust that feeling! There will be plenty of time to get the theory down. Today we're just going to learn how to use them practically.

Functions that return Actions
-----------------------------

`putStrLn` is a function that prints out a message to the console, right? Not quite. Let's look at its type. What does it return?

    putStrLn :: String -> IO ()

It's a function that accepts a string, and *returns an action to print something out to the console*. That's an important distinction. `putStrLn` doesn't actually print anything, it returns an instruction to print something. Let's call those instructions *actions*.

What good are actions?
----------------------

What can we do with actions? They are values, like `String` or `Bool`, just fancier. Think of them as Todo list items. We can mix them together, and combine them into larger actions. When you use `do` syntax, that's exactly what you're doing:

    main :: IO ()
    main = do
      putStrLn "Go shopping"
      putStrLn "Take a nap"
      putStrLn "Learn Haskell"

Look at the type: `IO ()` means that main is one `IO` action. We've combined 3 actions into one.

Calling the function doesn't perform the action
-----------------------------------------------

What will it print out if I do this?

    main = do
      let action = putStrLn "Hello World"
      return ()

Calling `putStrLn` does not print anything, it returns a value of type `IO ()` which can be performed later. In the above, we are never executing it.

Anything at the same indentation level (except `let`) in a do-block is expected to be an action of the same type, and will be executed when the parent is.

    -- what will this do?
    main = do
      let action = putStrLn "Hello World"
      action
      action
      action
      return ()

We can see this from the type. When we call `putStrLn` with one argument, it returns `IO ()`, or an `IO` action, all ready to be used.

    putStrLn :: String -> IO ()

    action :: IO ()
    action = putStrLn "Hello World"

Actions can have results
------------------------

Ok, now we know that actions are values. Those actions can sometimes yield something when they are performed. Let's look at the type of `getLine`. What does it return?

    getLine :: IO String

It doesn't return a `String`, it returns an action, right? Then what does that `String` mean? It means that *when the action is performed, it will yield a `String`*. Let's call that the *result* of the action.

You can use result from an action with the `<-` operator.

    main = do
      name <- getLine
      putStrLn ("Hello " ++ name)

`name` is no longer an action. Its type is just `String`, not `IO String` like `getLine` was. It's the difference between a plan to go to the grocery store (`getLine`) and actually coming back with the groceries (`name`)

A do-block always yields whatever its last action does. If we put `return` last, we can make it yield any simple value.

    sayHello :: IO String
    sayHello = do
      name <- getLine
      putStrLn ("Hello " ++ name)
      return name

Different types of Actions
--------------------------

Within a given do-block, all actions must be the same type. So for an `IO` block like what `main` returns, every line has to have the type `IO a`. You can't put regular values there (without let), because they aren't actions.


    main :: IO ()
    main = do
      -- good. putStrLn "Hello" is of type IO ()
      putStrLn "Hello"

      -- error! String is not an IO a!
      "whatever"

Remember that functions like `putStrLn` aren't `IO` actions, they *return* `IO` actions. So this won't work:
nor can you put other actions there.

    main = do

      -- error! putStrLn is a function with type String -> IO ()
      putStrLn

      -- good. If we give it one argument, it's now an IO action
      putStrLn "Hello"

Let's look at some examples of different kinds of monads, each with their own actions. To use the Maybe monad, each step must be of type `Maybe a`. You can't mix different kinds of actions in the same do block.

    doesNotWork :: Maybe Int
    doesNotWork = do

      -- these work, because they are all Maybe a
      Just 6
      Nothing

      -- error! this is type IO (), not Maybe a!
      putStrLn "Hello"

      return 5

    main = do
      -- error! this is type Maybe Int, not IO a
      Just 6

Monads decide how to combine actions
------------------------------------

Each type of Monad combines actions differently. We already looked at `IO`. It performs the actions in order with `IO` side effects. `Maybe`, on the other hand, specifies that if any step is `Nothing`, the whole thing stops and exits with a `Nothing`.

    beCareful :: Maybe Int
    beCareful = do
      Just 6
      Nothing
      return 5

The do-block here always yields `Nothing`, because of that `Nothing` in the second to last step.

Time to code
------------

Add `beCareful` and `sayHello` to `src/Main.hs`

    $ stack ghci
    Prelude> :load src/Main.hs

    *Main> beCareful
    Nothing

    *Main> sayHello
    woot
    Hello woot
    "woot"

Now try mixing them up. Add an `IO` action into `beCareful`.

    beCareful :: Maybe Int
    beCareful = do
      Just 6
      putStrLn "oops"
      Nothing
      return 5

What happens when we reload GHCI?

    *Main> :r
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )

    src/Main.hs:11:3:
        Couldn't match expected type ‘Maybe a0’ with actual type ‘IO ()’
        In a stmt of a 'do' block: putStrLn "oops"
        In the expression:
          do { Just 6;
              putStrLn "oops";
              Nothing;
              return 5 }
    Failed, modules loaded: none.

Hopefully that error message makes sense to you now.

Other monads in the wild
------------------------

The Parsec libary uses `Parser` actions to define a grammer for parsing. Here's one to parse an IPv4 address. It combines actions by matching them against an input. If the input doesn't match any of the steps, it exits and tells its parent it didn't match.

    data IP = IP Word8 Word8 Word8 Word8 deriving Show

    parseIP :: Parser IP
    parseIP = do
      d1 <- decimal
      char '.'
      d2 <- decimal
      char '.'
      d3 <- decimal
      char '.'
      d4 <- decimal
      return $ IP d1 d2 d3 d4

Can you guess what the type of `decimal` and `char` are?

    char :: Parser Char
    decimal :: Parser Word8

The Scotty library uses the `ScottyM` monad to define a web API. Here each action is combined to create an API description which is later matched against incoming HTTP requests. The first ones are given priority when matching.

    routes :: ScottyM ()
    routes = do
      get "/"      (text "homepage!")
      get "/hello" (text "hello")

Don't sweat the little stuff
----------------------------

So there you go. Don't worry about it. You can use `do` notation, monads, and write programs. You don't have to understand exactly what monads are doing under the hood to have an intuition for how to use them. Later when you learn the theory, it'll be much easier if you've been using them.

Assignment
----------

Build a program that asks the user for a message and a number on the command line, and print out that message N times. Use the control functions from [`Control.Monad`][control-monad], like `mapM`, or `replicateM`

Read [Input and Output](http://learnyouahaskell.com/input-and-output) from Learn You a Haskell. Implement the unix `cat` command, which takes a command-line argument as a path to a file and prints out its contents. Refer to [Getting Started][getting-started] to see how to use `stack exec`.

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[using-monads]: http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
[json-api]: http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
[lyah]: http://learnyouahaskell.com/chapters

[control-monad]: https://hackage.haskell.org/package/base/docs/Control-Monad.html
