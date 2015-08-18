---
layout: post
title: Practical Haskell - Getting Started
---

{{ page.title }}
================

Haskell is famous for having a steep learning curve. As a web developer we're used to clear tutorials that we can understand and complete within an hour or two. Haskell introduces many new concepts not found in other languages, but we can learn it faster by spending as much time coding as we do reading.

This is the first of a tutorial series intended to introduce Haskell by coding things that work.

1. [**Getting Started**][getting-started]
2. [Importing Code][importing-code]
3. [Using Monads][using-monads]
4. [Build a JSON API][json-api]

In this article we will show you how to get Haskell installed, how to set up a new project, and run your code.

Tools and Names
---------------

*GHC* is the compiler for Haskell. It takes Haskell source code and turns it into an executable.

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

We can Use the `:load` command to load our code. Note that you can tab-complete module names.

    Prelude> :load Main
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

Building an Executable
----------------------

When you are ready to ship, you can build an executable with `stack build`

    $ stack build

It will tell you where the executable is, but it's easier to run it with `stack exec`. It will run anything in your `main` function.

    $ stack exec my-project
    Hello bobby!
    Hello World!


What's next
-----------

In the next article, we should you how to [import code from other modules][importing-code].

Other resources:

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) - Good (free) introductory Haskell book.
* [Prelude Documentation][prelude] - All the functions that come built in
* [Complete source code][source] for this tutorial
* Configure your text editor to underline errors for you. Check out [Editor Setup][editor-setup]

Assignment
----------

1. Read [chapter 3 of Learn You a Haskell](http://learnyouahaskell.com/types-and-typeclasses) and add type definitions to `main` and `greet`

2. Use the `getLine` function to read a name from the command-line, and print out a greeting to that name. Will require using the Prelude Documentation, and probably some googling.

[Answers](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/Assignments.hs)

[stack]: https://github.com/commercialhaskell/stack
[source]: https://github.com/seanhess/practical-haskell/tree/master/01-getting-started
[editor-setup]: /2015/08/05/practical-haskell-editors.html
[prelude]: https://hackage.haskell.org/package/base/docs/Prelude.html
[base]: http://hackage.haskell.org/package/base
[lyah]: http://learnyouahaskell.com/chapters

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[using-monads]: http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
[json-api]: http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html

