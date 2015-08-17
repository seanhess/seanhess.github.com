---
layout: post
title: Practical Haskell - Getting Started
---

{{ page.title }}
================

Haskell is famous for having a steep learning curve. As a web developer we're used to clear tutorials that we can understand and complete within an hour or two. Haskell introduces many new concepts not found in other languages, but we can learn faster by spending as much time coding as we do reading.

This is the first of a tutorial series intended to introduce Haskell by coding things that work.

In this article we will show you how to get Haskell installed, how to set up a new project, and run your code.

Tools and Names
---------------

*GHC* is the semi-official compiler for Haskell. It takes Haskell source code and turns it into an executable.

*Cabal* is the package description format. You'll have a file called `my-project.cabal` with information about your project. There's an executable called cabal too, but we are going to use stack instead.

[*Stack*][stack] is the package manager. It reads `my-project.cabal` and `stack.yaml` to link in third party code. It will install GHC for you too.

Installing Stack
----------------

The only thing you need to download is Stack. It will install everything else for you.

* [Download Stack](https://github.com/commercialhaskell/stack#how-to-install)

Then, follow the instructions on the download page for your operating system. Here's what I did on a mac:

1. Unzip the file by double clicking it
2. Move it to `/usr/local/bin`

       $ mv stack-0.1.3.1-x86_64-osx /usr/local/bin/stack

3. Give it executable permissions

       $ chmod +x /usr/local/bin/stack

4. Check to make sure it is working

       $ stack --version
       Version 0.1.3.1, Git revision 908b04205e6f436d4a5f420b1c6c646ed2b804d7

Setting up a new project
------------------------

We're going to need a few files to get our first project going: some haskell source code, a `stack.yaml` and a `.cabal` file.

First, let's make a new directory

    $ mkdir my-project
    $ cd my-project

Now create a file called `my-project.cabal` and paste this in. Another day we'll edit this file to specify dependencies on other people's code.

    name:                my-project
    version:             0.1.0.0
    synopsis:            My first Haskell Project
    description:         Please see README.md
    license:             BSD3
    author:              Your name here
    maintainer:          your.address@example.com
    category:            Web
    build-type:          Simple
    cabal-version:       >=1.10

    executable my-project
      hs-source-dirs:      src
      main-is:             Main.hs
      default-language:    Haskell2010
      build-depends:       base

Next let's create a [`stack.yaml`](https://github.com/commercialhaskell/stack/wiki/stack.yaml) file in the same directory. The resolver tells stack which version of GHC and your dependencies to use.

    flags: {}
    packages:
    - '.'
    extra-deps: []
    resolver: lts-3.1

Finally, create a file `src/Main.hs` with your source code:

    main = do
      putStrLn "Hello"
      putStrLn "World!"

Your project folder should now look like this:

    my-project.cabal
    stack.yaml
    src/
      Main.hs

[Here's the full source if you get stuck][source].

Installing GHC
---------------

Stack will install the correct version of GHC for our project. This is cool because everyone working on your project will be on the same version. Let's give it a shot: run this in your project folder

    $ stack setup

Which results in:

    Downloaded lts-3.1 build plan.    
    Caching build plan
    Fetched package index.
    Populated index cache.
    Downloaded ghc-7.10.2.
    Installed GHC.
    stack will use a locally installed GHC
    For more information on paths, see 'stack path' and 'stack exec env'
    To use this GHC and packages outside of a project, consider using:
    stack ghc, stack ghci, stack runghc, or stack exec

Run the Code!
-------------

Now we're ready to run some code! Let's use stack to fire up GHCI: the Haskell [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

    $ stack ghci
    Configuring GHCi with the following packages: my-project
    GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
    Prelude> 

There are [other great tutorials that will teach you how to use ghci](http://learnyouahaskell.com/starting-out), but here are a few examples:

    Prelude> 2 + 15
    17

    Prelude> 5 == 5
    True

Use the `:load` command to load our code:

    Prelude> :load src/Main.hs
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )
    Ok, modules loaded: Main.

Then we can run our program by typing `main`

    Main> main
    Hello
    World!

Making Changes
--------------

We can use ghci to test our changes as we go. Let's make a `greet` function! Add this to `src/Main.hs`

    greet name = "Hello " ++ name ++ "!"

Now go back to ghci and type `:r`

    Main> :r
    [1 of 1] Compiling Main   (/Users/seanhess/projects/practical-haskell/getting-started/src/Main.hs, interpreted )
    Ok, modules loaded: Main.

We can test `greet` without it being used in `main`

    Main> greet "bobby"
    "Hello bobby!"

Let's add it to our main program! Edit `src/Main.hs`

    main = do
      putStrLn (greet "bobby")
      putStrLn (greet "World")

Reload again with `:r` and then run `main`

    Main> :r
    Main> main
    Hello bobby!
    Hello World!

Importing other code
--------------------

Haskell's core library, called [base][base], contains many useful functions. Many of the most common functions are in the [Prelude][prelude], and are automatically imported in your code. `putStrLn` is one of these. A few others are `+`: the addition operator, and `show`: which converts something to a `String`.

    printNumbers = do
      putStrLn (show (3+4)) 

Only the [Prelude][prelude] is imported automatically. If you want to use anything else, you need to import it. Imports belong at the top of your program. Let's import `readFile` and write a program that prints out our stack config file

    import System.IO (readFile)

    printConfig = do
      contents <- readFile "stack.yaml"
      putStrLn contents

Go ahead and add the above to `src/Main.hs`. Reload them in GHCI with `:r`, and then run them by typing their names:

    *Main> printNumbers
    7

    *Main> printConfig
    flags: {}
    ... etc

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

    Prelude> :load src/Main.hs
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )
    Ok, modules loaded: Main.

    *Main> printTime
    2015-08-17 18:41:55.068122 UTC

You can add this to your main function if you want, as before. In `src/Main.hs`

    main = do
      putStrLn (greet "bobby")
      putStrLn (greet "World")
      printTime

Building an Executable
----------------------

When you are ready to ship, you can build an executable with `stack build`

    $ stack build

It will tell you where the executable is, but it's easier to run it with `stack exec`. It will run anything in your `main` function.

    $ stack exec my-project
    Hello bobby!
    Hello World!
    2015-08-17 18:41:55.068122 UTC

Text Editor Integration
-----------------------

You can configure your text editor to underline errors for you. Check out [Editor Setup][editor-setup]

Where to go from here
---------------------

Hopefully I'll add more tutorials in the series soon, but here are some resources to keep you busy.

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) - Good (free) introductory Haskell book.
* [Prelude Documentation][prelude] - All the functions that come built in
* [Complete source code][source] for this tutorial

Assignment
----------

1. Read [chapter 2 of Learn You a Haskell](http://learnyouahaskell.com/starting-out) and make your program show the result of adding two numbers.

2. Read [chapter 3 of Learn You as Haskell](http://learnyouahaskell.com/types-and-typeclasses) and add type definitions to `main` and `greet`

3. Use the `getLine` function to read a name from the command-line, and print out a greeting to that name. Will require using the Prelude Documentation, and probably some googling.

4. Keep hacking on this project and make something of your own. Please share a link in the comments.

Answers: [numbers](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/AssignmentNumbers.hs) [getLine](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/AssignmentGetLine.hs)

[stack]: https://github.com/commercialhaskell/stack
[source]: https://github.com/seanhess/practical-haskell/tree/master/01-getting-started
[editor-setup]: /2015/08/05/practical-haskell-editors.html
[prelude]: https://hackage.haskell.org/package/base/docs/Prelude.html
[base]: http://hackage.haskell.org/package/base
