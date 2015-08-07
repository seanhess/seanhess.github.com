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
    Using latest snapshot resolver: lts-2.21
    Writing global (non-project-specific) config file to: /Users/seanhess/.stack/global/stack.yaml
    Note: You can change the snapshot via the resolver field there.
    Downloading lts-2.21 build plan ...

We need to change the resolver in our global `stack.yaml` file so we get the latest version of hdevtools. Open `~/.stack/global/stack.yaml` and change the resolver field to a recent nightly build.

    flags: {}
    packages: []
    extra-deps: []
    resolver: nightly-2015-08-07

Now run stack setup again

    $ stack setup

Then install hdevtools

    $ stack install hdevtools
    Using resolver: nightly-2015-08-07 from global config file: /Users/seanhess/.stack/global/stack.yaml
    Copying from /Users/seanhess/.stack/snapshots/x86_64-osx/nightly-2015-08-07/7.10.2/bin/hdevtools to /Users/seanhess/.local/bin/hdevtools

    Installed executables to /Users/seanhess/.local/bin/:
    - hdevtools

We need to add `~/.local/bin` to our PATH so our text editor can find the `hdevtools` executable. On OSX, open `~/.bash_profile` and add this line:

    export PATH=~/.local/bin:$PATH

Now open a new terminal window and see if it worked!

    $ hdevtools --version
    hdevtools: version 0.1.1.9 (ghc-7.10.2-x86_64-darwin, cabal-1.22.4.0)

Getting a Text Editor Plugin
----------------------------

The hdevtools homepage has a list of [available editor plugins](https://github.com/schell/hdevtools#text-editor-integration), including Vim, Emacs, and Atom. Follow the instructions to get set up!

You should be able to get errors highlighting on save, and see what the type is for different parts of code. Here's what it looks like in my editor:

<img src="http://i.imgur.com/iKpRqPS.gifv"/>

Limitations
-----------

Hdevtools doesn't provide any autocomplete information.

You can't use Atom's Haskell-IDE until ghc-mod gets up to speed.

Assignment
----------

Play around with the various features your editor provides on the project you created in [Getting Started][getting-started]

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[ghc-mod]: http://www.mew.org/~kazu/proj/ghc-mod/en/
[hdevtools]: https://github.com/schell/hdevtools/
[stack]: https://github.com/commercialhaskell/stack
