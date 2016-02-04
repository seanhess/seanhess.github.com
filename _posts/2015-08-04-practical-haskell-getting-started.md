---
layout: post
title: Practical Haskell - Getting Started with Stack
---

{{ page.title }}
================

Haskell is famous for having a steep learning curve. As a web developer we're used to clear tutorials that we can understand and complete within an hour or two. Haskell introduces many new concepts not found in other languages, but we can learn it faster by spending as much time coding as we do reading.

This is the first of a tutorial series intended to introduce Haskell by coding things that work.

1. [**Getting Started with Stack**][getting-started]
2. [Importing Code][importing-code]
3. [Using Monads][using-monads]
4. [Build a JSON API][json-api]

In this article we will show you how to get Haskell installed, how to set up a new project, and run your code.

Tools and Names
---------------

*GHC* is the compiler for Haskell. It takes Haskell source code and turns it into an executable.

*Cabal* is the package description format. You'll have a file called `my-project.cabal` with information about your project. There's an executable called cabal too, but we are going to use stack instead.

[*Stack*][stack] is a package manager. It reads `my-project.cabal` and `stack.yaml` to link in third party code. It will install GHC for you too.

Installing Stack
----------------

The only thing you need to download is Stack. It will install everything else for you.

* <a href="https://github.com/commercialhaskell/stack#how-to-install" target="_blank">Download Stack</a>

Then, follow the instructions on the download page for your operating system. Here's what I did on a mac:

1. Unzip the file by double clicking it
2. Move it to `/usr/local/bin`

       $ mv stack-0.1.3.1-x86_64-osx /usr/local/bin/stack

3. Give it executable permissions

       $ chmod +x /usr/local/bin/stack

Alternatively on OSX you can install via homebrew:

    brew update
    brew install haskell-stack

Check to make sure it is working

    $ stack --version
    Version 0.1.3.1, Git revision 908b04205e6f436d4a5f420b1c6c646ed2b804d7


Setting up a new project
------------------------

We're going to need a few files to get our first project going: some haskell source code, a `stack.yaml` and a `.cabal` file. We can create these files by hand, but stack has a template feature we can use instead. Let's call our project "my-project" and use the `simple` template.

    $ stack new my-project simple
    ...

This creates a directory named `my-project`. Let's see what's inside:

    LICENSE
    Setup.hs
    stack.yaml
    my-project.cabal
    src/
      Main.hs

The [`stack.yaml`](https://github.com/commercialhaskell/stack/wiki/stack.yaml) config file tells stack which version of GHC and your dependencies to use.

    flags: {}
    packages:
    - '.'
    extra-deps: []
    resolver: lts-3.1

We use the `my-project.cabal` config file to store settings like the project name, and license. In the next article, [Importing Code][importing-code], we'll edit this file to add dependencies.

    name:                my-project
    version:             0.1.0.0
    synopsis:            Simple project template from stack
    description:         Please see README.md
    homepage:            http://github.com/githubuser/my-project#readme
    license:             BSD3
    license-file:        LICENSE
    author:              Sean Hess
    maintainer:          seanhess@gmail.com
    copyright:           2010 Author Here
    category:            Web
    build-type:          Simple
    cabal-version:       >=1.10

    executable my-project
      hs-source-dirs:      src
      main-is:             Main.hs
      default-language:    Haskell2010
      build-depends:       base >= 4.7 && < 5

Last but not least, we have some source code. `src/Main.hs` is the main module for our program. This is where the Haskell happens.

~~~ haskell
module Main where

main :: IO ()
main = do
  putStrLn "hello world"
~~~

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
    hello world

Making Changes
--------------

We can use ghci to test our changes as we go. Let's make a `greet` function! Add this to `src/Main.hs`

~~~ haskell
greet name = "Hello " ++ name ++ "!"
~~~

Now go back to ghci and type `:reload` or `:r`

    Main> :r
    [1 of 1] Compiling Main   (src/Main.hs, interpreted)
    Ok, modules loaded: Main.

We can test `greet` without it being used in `main`

    Main> greet "bobby"
    "Hello bobby!"

Let's add it to our main program! Edit `src/Main.hs`

~~~ haskell
main = do
  putStrLn (greet "bobby")
  putStrLn (greet "World")
~~~

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

Other Resources
-----------

* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) - Good (free) introductory Haskell book.
* [Prelude Documentation][prelude] - All the functions that come built in
* [Complete source code][source] for this tutorial
* Configure your text editor to underline errors for you. Check out [Editor Setup][editor-setup]

Assignment
----------

1. Read [chapter 3 of Learn You a Haskell](http://learnyouahaskell.com/types-and-typeclasses) and add a type declaration to `greet`

2. Use the `getLine` function to read a name from the command-line, and print out a greeting to that name. Will require using the Prelude Documentation, and probably some googling.

[Answers](https://github.com/seanhess/practical-haskell/blob/master/01-getting-started/src/Assignments.hs)

What's Next
-----------

In the next article, [Importing Code][importing-code], we show you how to use other built-in modules and 3rd party code.

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

