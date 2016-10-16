---
layout: post
title: Practical Haskell - Editor Setup with Stack
---

{{ page.title }}
================

In [Getting Started with Stack][getting-started] we showed you how to install GHC and [stack][stack], set up a project, and write a little code.

We can use GHCI without editor integration for a pretty good workflow, but configuring your text editor to underline errors can be very helpful. This article will show you how to get it all working with [stack][stack].

The bleeding edge
-----------------

There are two main editor plugins: [ghc-mod][ghc-mod] and [hdevtools][hdevtools]. ghc-mod is great, ~~but at the time of this writing [they don't support stack](https://github.com/kazu-yamamoto/ghc-mod/issues/498), so we can't use them with the project setup we introduced in [Getting Started][getting-started].~~ But hdevtools works great!

**Update**: ghc-mod has been updated to work with stack. For atom users, I'd now recommend using [ide-haskell](https://atom.io/packages/ide-haskell), which should have instructions to install all dependencies. Then refer to this article: [Using ghc-mod with stack](https://github.com/atom-haskell/haskell-ghc-mod/wiki/Using-with-stack). Basically you install ghc-mod locally for each project:

    cd my-project
    stack install ghc-mod


Installing GHC and Hdevtools globally
-------------------------------------

Normally, we want to use [stack][stack] in a project folder with a `stack.yaml` file and a `.cabal` file so we can install specific dependencies to each project. But our text editor runs globally, so we need to install hdevtools globally instead.

First, head into a global folder that isn't a haskell project and run `stack setup`. This will install ghc for you.

    $ cd ~
    $ stack setup
    Run from outside a project, using implicit global config
    Using resolver: lts-3.1 from global config file: /Users/seanhess/.stack/global/stack.yaml
    stack will use a locally installed GHC
    For more information on paths, see 'stack path' and 'stack exec env'
    To use this GHC and packages outside of a project, consider using:
    stack ghc, stack ghci, stack runghc, or stack exec

Then install hdevtools

    $ stack install hdevtools
    Run from outside a project, using implicit global config
    Using resolver: lts-3.1 from global config file: /Users/seanhess/.stack/global/stack.yaml
    NOTE: the install command is functionally equivalent to 'build --copy-bins'
    syb-0.5.1: download
    ...
    Completed all 5 actions.
    Copying from /Users/seanhess/.stack/snapshots/x86_64-osx/lts-3.1/7.10.2/bin/hdevtools to /Users/seanhess/.local/bin/hdevtools

    Copied executables to /Users/seanhess/.local/bin/:
    - hdevtools

Adding it to your PATH
----------------------

We need both `hdevtools` and `ghc` to be available on our `PATH` environment variable for it to work with our text editor. We can use `stack path` to have stack tell us where it puts binaries:

    $ cd ~
    $ stack path
    Run from outside a project, using implicit global config
    Using resolver: lts-3.1 from global config file: /Users/seanhess/.stack/global/stack.yaml
    global-stack-root: /Users/seanhess/.stack
    ...
    ghc-paths: /Users/seanhess/.stack/programs/x86_64-osx
    local-bin-path: /Users/seanhess/.local/bin
    ...

`ghc` is located in a folder under `GHC-PATHS/ghc-VERSION/bin`, like this on my system:

    ~/.stack/programs/x86_64-osx/ghc-7.10.2/bin/ghc

`hdevtools` is located in the `local-bin-path` folder:

    ~/.local/bin

Let's add both to our PATH. On OSX, open `~/.bash_profile` and add something like this:

    export PATH=~/.stack/programs/x86_64-osx/ghc-7.10.2/bin/ghc:~/.local/bin:$PATH

Testing to see if it works
--------------------------

Let's open new terminal window to get our PATH to reload and type the following:

    $ hdevtools --version
    hdevtools: version 0.1.2.1 (ghc-7.10.2-x86_64-darwin, cabal-1.22.4.0)

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.10.2

Now let's make sure hdevtools works in our project. Go to a haskell project and use the `hdevtools check` command. This is what your editor will be doing behind the scenes.

    $ cd ~/my-haskell-project
    $ hdevtools check src/Main.hs

Getting a Text Editor Plugin
----------------------------

Now that `hdevtools` is working on the command-line we can install an editor plugin. The hdevtools homepage has a list of [available editor plugins](https://github.com/schell/hdevtools#text-editor-integration), including Vim, Emacs, and Atom. Follow the instructions to get set up!

You should be able to get errors highlighting on save, and see what the type is for different parts of code. Here's what it looks like in my editor as I edit code and hit save.

<img src="http://i.imgur.com/iKpRqPS.gif"/>

Limitations
-----------

Hdevtools doesn't provide any autocomplete information.

~~You can't use Atom's Haskell-IDE until ghc-mod gets up to speed. The error highlighting provided by the other plugins are the most useful feature though.~~

GHC-mod now supports stack. Refer to the instructions at the beginning of the article for atom users.

Assignment
----------

Play around with the various features your editor provides on the project you created in [Getting Started][getting-started]

Next
----

Next up, let's [learn how to import code][importing-code]

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[ghc-mod]: http://www.mew.org/~kazu/proj/ghc-mod/en/
[hdevtools]: https://github.com/hdevtools/hdevtools/
[stack]: https://github.com/commercialhaskell/stack
