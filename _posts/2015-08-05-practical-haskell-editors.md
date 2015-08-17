---
layout: post
title: Practical Haskell - Editor Setup with Stack
---

{{ page.title }}
================

In [Getting Started][getting-started] we showed you how to install GHC and [stack][stack], set up a project, and write a little code.

We can use GHCI without editor integration for a pretty good workflow, but configuring your text editor to underline errors can be very helpful. This article will show you how to get it all working with [stack][stack].

The bleeding edge
-----------------

There are two main editor plugins: [ghc-mod][ghc-mod] and [hdevtools][hdevtools]. ghc-mod is great, but at the time of this writing [they don't support stack](https://github.com/kazu-yamamoto/ghc-mod/issues/498), so we can't use them with the project setup we introduced in [Getting Started][getting-started]. But hdevtools works great!

Installing Hdevtools globally
-----------------------------

Normally, we want to use [stack][stack] in a project folder with a `stack.yaml` file and a `.cabal` file so we can install specific dependencies to each project. But our text editor runs globally, so we need to install hdevtools globally instead.

First, head into a global folder that isn't a haskell project and run `stack setup`

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

We need to add `~/.local/bin` to our PATH so our text editor can find the `hdevtools` executable. On OSX, open `~/.bash_profile` and add this line:

    export PATH=~/.local/bin:$PATH

Now open a new terminal window and see if it worked!

    $ hdevtools --version
    hdevtools: version 0.1.2.1 (ghc-7.10.2-x86_64-darwin, cabal-1.22.4.0)

Getting a Text Editor Plugin
----------------------------

The hdevtools homepage has a list of [available editor plugins](https://github.com/schell/hdevtools#text-editor-integration), including Vim, Emacs, and Atom. Follow the instructions to get set up!

You should be able to get errors highlighting on save, and see what the type is for different parts of code. Here's what it looks like in my editor as I edit code and hit save.

<img src="http://i.imgur.com/iKpRqPS.gif"/>

Limitations
-----------

Hdevtools doesn't provide any autocomplete information.

You can't use Atom's Haskell-IDE until ghc-mod gets up to speed. The error highlighting provided by the other plugins are the most useful feature though.

Assignment
----------

Play around with the various features your editor provides on the project you created in [Getting Started][getting-started]

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[ghc-mod]: http://www.mew.org/~kazu/proj/ghc-mod/en/
[hdevtools]: https://github.com/schell/hdevtools/
[stack]: https://github.com/commercialhaskell/stack
