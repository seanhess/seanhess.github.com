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

* Unzip the file by double clicking it
* Move it to `/usr/local/bin`

    > mv stack-0.1.2.0-x86_64-osx /usr/local/bin/stack

* Give it executable permissions

    > chmod +x /usr/local/bin/stack

* Check to make sure it is working

    > stack --version
    Version 0.1.2.0, Git revision 65246552936b7da4b64b38372feac903d96a8911

Setting up a new project
------------------------

We're going to need a few files to get our first project going: some haskell source code, a `stack.yaml` and a `.cabal` file.

First, let's make a new directory

    > mkdir my-project
    > cd my-project

Now create a file called `my-project.cabal` and paste this in:

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

Then let's create a `stack.yaml` file in the same directory.

    flags: {}
    packages:
    - '.'
    extra-deps: []
    resolver: nightly-2015-06-16

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

Installing Dependencies and GHC
-------------------------------

Stack will install the correct version of GHC for our project. Let's give it a shot: run this in your project folder

    > stack setup

Which results in:

    Downloaded nightly-2015-06-16 build plan.    
    Caching build plan
    Fetched package
    Populated index cache.
    Downloaded ghc-7.10.2.                   
    Installed GHC.     
    Would add the following to PATH: /Users/seanhess/.stack/programs/x86_64-osx/ghc-7.10.2/bin

It says we need to add that folder to our path, so let's do that. On my mac, I add the following line to `~/.bash_profile`

    export PATH=/Users/seanhess/.stack/programs/x86_64-osx/ghc-7.10.2/bin:$PATH

Then open a new terminal window to get it to reload and we're ready!

Run the Code!
-------------

Now we're ready to run some code! Let's fire up the REPL, called ghci

    > stack ghci
    Configuring GHCi with the following packages: my-project
    GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
    Prelude> 

There are [other great tutorials that will teach you how to use ghci](http://learnyouahaskell.com/starting-out), but here are a few examples:

    Prelude> 2 + 15
    17

    Prelude> 5 == 5
    True

Use the `:load` command to load our code:

    Prelude> :load Main
    [1 of 1] Compiling Main             ( src/Main.hs, interpreted )
    Ok, modules loaded: Main.

Then we can run our program by typing `main`

    Main> main
    Hello
    World!

Making Changes
--------------

We can use ghci to test our changes as we go. Let's make a `hello` function! Add this to `src/Main.hs`

    hello name = "Hello " ++ name

Now go back to ghci and type `:r`

    Main> :r
    [1 of 1] Compiling Main   (/Users/seanhess/projects/practical-haskell/getting-started/src/Main.hs, interpreted )
    Ok, modules loaded: Main.

We can test `hello` without it being used in `main`

    Main> hello "bobby"
    "Hello bobby"

Let's add it to our main program! Edit `src/Main.hs`

    main = do
      putStrLn (hello "bobby")
      putStrLn (hello "World!")

Reload again with `:r` and then run `main`

    Main> :r
    Main> main
    Hello bobby
    Hello World!

Building an Executable
----------------------

When you are done, you can build an executable with `stack build`

    stack build

It will tell you where the executable is, but it's usually easier to run it with `stack exec`

    stack exec my-project

Where to go from here
---------------------

Hopefully I'll add more tutorials in the series soon, but here are some resources to keep you busy.

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) - Good (free) introductory Haskell book.
* [Prelude Documentation](https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html) - All the functions that come built in
* [Complete source code][source] for this tutorial

Assignment
----------

Read [chapter 2 of Learn You a Haskell](http://learnyouahaskell.com/starting-out) and make your program show the result of adding two numbers.

Use the `getLine` function to read a name from the command-line, and print out a message saying hello to that name. Will require using the Prelude Documentation, and probably some googling.

Answers: [numbers](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/AssignmentNumbers.hs) [getLine](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/AssignmentGetLine.hs)




[stack]: https://github.com/commercialhaskell/stack
[source]: https://github.com/seanhess/practical-haskell/tree/master/01-getting-started
