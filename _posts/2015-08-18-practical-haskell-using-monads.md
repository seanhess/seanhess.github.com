---
layout: post
title: Practical Haskell - Using Monads
---

{{ page.title }}
================

This article is part of a tutorial series intended to introduce Haskell by coding things that work.

1. [Getting Started][getting-started]
2. [Importing Code][importing-code]
3. [**Using Monads**][self]

In this article we will teach you how to use Monads without panicking.

Before you Begin
----------------

This article assumes a basic understanding of Haskell Types. You should know how to add a type declaration to a simple function, and understand the difference between `String` `Maybe String` and `[String]`. Consider reading [Chapter 8 of Learn You a Haskell: Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses). You'll probably need to have read Chapters 2 and 3 first.

Don't Panic
-----------

Monads are not scary. You've been using them since the beginning of this tutorial. Whenever we make an `IO` function, and use `do`, we're using monads. Let's take a look at a simple example:

    main = do
      name <- getLine
      putStrLn ("Hello, " ++ name)

You probably have a pretty good feel for what this is doing. Trust that feeling! There will be plenty of time to get the theory down. Today we're just going to learn how to use them practically.

Actions vs Functions
---------------------

`putStrLn` is a function that prints out a message to the console, right? Not quite. Let's look at it's type. What does it return?

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

Look at the type of `main`. `IO ()` means that main is one `IO` action. We've combined 3 actions into one.

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

Ok, so now we understand that actions are values. Those actions can sometimes yield something when they are performed. Let's look at the type of `getLine`. What does it return?

    getLine :: IO String

It doesn't return a `String`, it returns an action, right? But what does that `String` mean, then? *It means that when the action is performed, it will yield a `String`*. Let's call that the *result* of the action.

You can use result from an action with the `<-` operator.

    main = do
      name <- getLine
      putStrLn ("Hello " ++ name)

`name` is no longer an action. It's type is just `String`, not `IO String` like `getLine` was. You can use it anywhere below.

A do-block always yields whatever its last action yields. We can use `return` to wrap a normal value, like a `String` into an action that does nothing other than yield it.

    sayHello :: IO String
    sayHello = do
      name <- getLine
      putStrLn ("Hello " ++ name)
      return name

Different types of Actions
--------------------------

Within a given do-block, all actions must be the same type So for an `IO` block like what `main` returns, every line has to have the type `IO a`. You can't put regular values there (without let), nor can you put other actions there.

    main :: IO ()
    main = do
      -- good. putStrLn "Hello" is of type IO ()
      putStrLn "Hello"

      -- error! String is not an IO a!
      "whatever"

      -- error! It's type is String -> IO ()
      putStrLn

Let's look at some examples of different kinds of monads, each with their own actions.

To use the Maybe monad, each step must be of type `Maybe a`.

    beCareful :: Maybe Int
    beCareful = do
      Just 6
      Nothing

      -- error! this is type IO (), not Maybe a!
      putStrLn "Hello"

      return 5

Monads decide how to combine actions
------------------------------------

Each type of Monad combines actions differently. We already looked at `IO`. It performs the actions in order with `IO` side effects. `Maybe`, on the other hand, specifies that if any step is `Nothing`, the whole thing stops and exits with a `Nothing`. The do-block here always yields `Nothing`, because of the second to last step.

    beCareful :: Maybe Int
    beCareful = do
      Just 6
      Nothing
      return 5

Other monads in the wild
------------------------

The Parsec libary uses `Parser` actions to define a grammer for parsing. Here's how to parse an IPv4 address. Can you guess what the type of `decimal` and `char` are? It combines actions by matching them against an input. If the input doesn't match any of the steps, it exits and tells its parent it didn't match.

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

    char :: Parser Char
    decimal :: Parser Word8

The Scotty library uses the `ScottyM` monad to define a web API. Here the steps are used to build an API description which is later matched against incoming requests. The first ones are given priority when matching

    routes :: ScottyM ()
    routes = do
      get "/"      (text "homepage!")
      get "/hello" (text "hello")

Don't sweat the little stuff
----------------------------

So there you go. Don't sweat it. You can use `do` notation, monads, and write programs. You don't have to understand exactly what monads are doing under the hood to have an intuition for how to use them.

Assignment
----------

Build a program that asks the user for a message and a number on the command line, and print out that message N times. Use the functions from [`Control.Monad`][control-monad].

[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[self]: http://seanhess.github.io/2015/08/18/practical-haskell-do-io.html
[lyah]: http://learnyouahaskell.com/chapters

[control-monad]: https://hackage.haskell.org/package/base/docs/Control-Monad.html
