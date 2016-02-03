---
layout: post
title: Practical Haskell - Importing Code
---

{{ page.title }}
================

This article is part of a tutorial series intended to introduce Haskell by coding things that work.

1. [Getting Started with Stack][getting-started]
2. [**Importing Code**][importing-code]
3. [Using Monads][using-monads]
4. [Build a JSON API][json-api]

In this article we're going to see how to include other code from Haskell's core libraries and from the web.

Before You Begin
----------------

In [Getting Started][getting-started] we showed you how to get everything installed and run some code. Before getting started make sure you follow the directions to [get stack installed](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html#installing-stack) and to [set up a project](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html#setting-up-a-new-project)

Importing other code
--------------------

Haskell's core library, called [base][base], contains many useful functions. Many of the most common functions are in the [Prelude][prelude], and are automatically imported in your code. `putStrLn` is one of these. A few others are `+`: the addition operator, and `show`: which converts something to a `String`.

    printNumbers = do
      putStrLn (show (3+4)) 

Only the [Prelude][prelude] module is imported automatically. If you want to use any other module, you need to import it. Imports belong at the top of your program. Let's import `readFile` from the module `System.IO` and write a program that prints out our stack config file

    import System.IO (readFile)

    printConfig = do
      contents <- readFile "stack.yaml"
      putStrLn contents

Go ahead and add both of the above functions to `src/Main.hs`, which you created in [Getting Started][getting-started]. Load them into GHCI and let's try it out.

    $ stack ghci
    Configuring GHCi with the following packages: my-project
    GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help

    Prelude> :load Main
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )
    Ok, modules loaded: Main.

    *Main> printNumbers
    7

    *Main> printConfig
    flags: {}
    packages:
    - '.'
    extra-deps: []
    resolver: lts-3.1

You can add them to your main program if you like. Edit `src/Main.hs` and add them to the `main` function.

    main = do
      putStrLn (greet "bobby")
      putStrLn (greet "World")
      printNumbers
      printConfig

If you get stuck, check out [the full source][source].

Finding 3rd party code
----------------------

[Base][base] is pretty cool, but the magic really happens when you import code that doesn't come standard. Haskell has a wealth of 3rd party modules. Try googling for something, like "Haskell Time". The first hit points us to the [time library](https://hackage.haskell.org/package/time).

On that page you can click to the homepage, or to the documentation for a particular module. Let's click into [Data.Time.Clock](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Clock.html). There's a function in there called `getCurrentTime` that gets the system clock time. Let's try to use it.

Importing 3rd party code
------------------------

Before we can use `getCurrentTime` we need to add the `time` package to our project. Open your `.cabal` file and add `time` to the build-depends field

    name:                my-project
    version:             0.1.0.0
    ...

    executable my-project
      ...
      build-depends:       base,
                           time

Head over to your terminal and run `stack build` to get stack to install it into your project.

    $ stack build
    ...

Now we can use `getCurrentTime` in a program. Add this to `src/Main.hs`

    import Data.Time (getCurrentTime)

    printTime = do
      time <- getCurrentTime
      putStrLn (show time)

Now restart ghci, reload it with main, and see what happens!

    $ stack ghci
    Configuring GHCi with the following packages: my-project
    GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help

    Prelude> :load 
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )
    Ok, modules loaded: Main.

    *Main> printTime
    2015-08-17 18:41:55.068122 UTC

You can add this to your main function if you want, as before. In `src/Main.hs`

    main = do
      putStrLn (greet "bobby")
      putStrLn (greet "World")
      printTime

[Source Code][source]

Stackage and Hackage
----------------------

[Hackage](http://hackage.haskell.org/) stores all versions of all haskell packages. But sometimes these are too bleeding edge. An author might publish a new version that doesn't work with another dependency in your project. Historically it has been very annoying to deal with this.

Stack fixes this problem by fetching our code from [Stackage][stackage], which hosts snapshots of common haskell packages known to work well together. Take a look at the resolver field of `stack.yml`. That lts-3.1 is one snapshot of haskell packages.

    flags: {}
    packages:
    - '.'
    extra-deps: []
    resolver: lts-3.1

So everything in lts-3.1 is knows to work together. You can see all the available packages in lts-3.1 right here: [https://www.stackage.org/lts-3.1][lts-3.1]

What if it's not on stackage?
-----------------------------

If you need the bleeding edge, or need to depend on a package not on stackage, you can add them to your `extra-deps` field in `stack.yaml`, and tell it exactly which version to use. You still need to add them to your `.cabal` file too.

For example, I use [RethinkDB](http://rethinkdb.com) for a database. The driver I use is [here on hackage](https://hackage.haskell.org/package/rethinkdb), but it isn't in [LTS Haskell 3.1][lts-3.1]. If I want to use it, I add it like this:

Stack.yaml

    flags: {}
    packages:
    - '.'
    extra-deps:
    - rethinkdb-2.0.0.0
    resolver: lts-3.1

my-project.cabal

    name:                my-project
    version:             0.1.0.0
    ...

    executable my-project
      ...
      build-depends:       base,
                           rethinkdb

Now anyone who clones my repository can just run `stack build` and get exactly the same dependencies as me.

Assignment
----------

Find a 3rd party module for JSON from [LTS Haskell](http://stackage.org/lts) or Google. Click through to the "Module documentation" to find a function to encode JSON. Write a program to JSON serialize the following list and print it out. Use `print` instead of `putStrLn`.

    numbers :: [Int]
    numbers = [1,2,3,4]

[Answer](https://github.com/seanhess/practical-haskell/blob/master/02-importing-code/src/Assignment.hs)

What's next
-----------

In the next tutorial, [Using Monads][using-monads], we show how to use monads without panicking

[stackage]: https://www.stackage.org/
[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[using-monads]: http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
[json-api]: http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
[lts]: http://stackage.org/lts
[lts-3.1]: http://stackage.org/lts-3.1

[prelude]: https://hackage.haskell.org/package/base/docs/Prelude.html
[base]: http://hackage.haskell.org/package/base
[source]: https://github.com/seanhess/practical-haskell/tree/master/02-importing-code
