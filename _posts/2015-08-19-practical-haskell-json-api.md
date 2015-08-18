---
layout: post
title: Practical Haskell - Building a JSON API
---

{{ page.title }}
================

This is part of a tutorial series intended to introduce Haskell by coding things that work. In this article we will be building a simple JSON API using [Scotty][scotty] and [Aeson][aeson].

1. [Getting Started][getting-started]
2. [Importing Code][importing-code]
3. [Using Monads][using-monads]
4. [**Build a JSON API**][json-api]

Before you Begin
----------------

This article assumes you have a project set up with stack, that you know how to import a 3rd party module, how to write a simple IO program, and how to write a simple function.

We recommend you work through all 3 previous tutorials. In [Getting Started][getting-started] we teach you how to build an run Haskell projects, in [Importing Code][importing-code] we show you how to import other modules, and in [Using Monads][using-monads] we teach you how to use `do` notation.

Installing Scotty
------------

[Scotty][scotty] is a haskell library that lets you make JSON APIs. It's a lot like [Express](http://expressjs.com/) or [Sinatra](http://www.sinatrarb.com/). You define routes and it makes a web server that will respond to HTTP requests.

Use the info from [Importing Code][importing-code] to install the `scotty` package, then run `stack build` to get stack to install it into your project. If this is the first time you've added a dependency, it might take a while, but next time it'll be faster.

    $ stack build
    ...
    Completed all 59 actions. 

A simple web server
-------------------

Let's start with a simple IO program. Replace `src/Main.hs` with the following

    main = do
      putStrLn "Starting Server..."

Before we can use it, we need to import scotty. Add this line to the top:

    import Web.Scotty
    main = do
      ...

Now, let's use the `scotty` function in `main` to start a scotty server:

    import Web.Scotty
    main = do
        putStrLn "Starting Server..."
        scotty 3000 $ do
            get "/hello" $ do
                text "hello world!"

Finally we need a GHC feature called OverloadedStrings that let's use string literals for other things (like routes, or text).

    {-# LANGUAGE OverloadedStrings #-}
    import Web.Scotty
    main = do
        putStrLn "Starting Server..."
        scotty 3000 $ do
            get "/hello" $ do
                text "hello world!"

Let's run this just like last time. Fire up `stack ghci`, load our code, and run `main`

    $ stack ghci
    Prelude> :load src/Main.hs
    *Main> main
    Starting Server...
    Setting phasers to stun... (port 3000) (ctrl-c to quit)

We can test it using our web browser: navigate to [http://localhost:3000/message](http://localhost:3000/message) and you should see the result:

    hello world!

What's with the nested do blocks?
---------------------------------

Remember from [Using Monads][using-monads] that each do block can only contain one type of actions. `main` always has IO actions, so both `putStrLn` and `scotty` must return an IO action.

    scotty :: Port -> ScottyM () -> IO ()

But it looks like in order to return an `IO` action, we need to pass a `ScottyM` action as the second parameter. Our program is equivalent to this:

    main = do
      putStrLn "Starting Server..."
      scotty 3000 someScottyMAction

That nested do block after the port number is just a `ScottyM` action. We could have done this instead:

    routes :: ScottyM ()
    routes = do
      get "/hello" $ do
        text "hello world!"

    main = do
      putStrLn "Starting Server..."
      scotty 3000 routes

The same thing goes for the handlers. Look at the type of `get`:

    get :: RoutePattern -> ActionM () -> ScottyM ()

Same thing again, we pass a `ActionM ()` as the last parameter. We could have written routes like this:

    routes :: ScottyM ()
    routes = do
      get "/hello" hello

    hello :: ActionM ()
    hello = do
      text "hello world!"

More Features: Route Parameters
-------------------------------

We can add route parameters with `:xxx` in the url, just like Sinatra and express. To read it in our handler, we use the [`param`](https://hackage.haskell.org/package/scotty-0.10.2/docs/Web-Scotty.html#g:4) function. Edit the `/hello` route to be this:

    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")

To get this to work we also need to import `<>`, which concatenates things that aren't strings. Add this to the top:

    import Data.Monoid ((<>))

Let's test! Go over to GHCI and type Ctrl-C to stop `main`. Then reload with `:r` and type `main` again. Check it out: [http://localhost:3000/hello/bob](http://localhost:3000/hello/bob). Try out a few different names for that last parameter

    hello bob!

If you get stuck, be sure to [check out the source][source]

Define Types for your API
-------------------------

We can make data types that describe our API. Here we have a `User` object with two fields. Add these to `src/Main.hs` at the top level.

    data User = User { userId :: Int, userName :: String } deriving (Show)

We can define some hard-coded users

    bob :: User
    bob = User { userId = 1, userName = "bob" }

    jenny :: User
    jenny = User { userId = 2, userName = "jenny" }

Or a list of users

    allUsers :: [User]
    allUsers = [bob, jenny]

Let's see if it works! Reload in GHCI.

    *Main> :r
    *Main> bob
    User { userId = 1, userName = "bob" }

Let's do JSON with Aeson
------------------------

The [Aeson][aeson] library lets you serialize data objects to JSON and vice versa. Let's install it the same way we did with scotty. Add it to `build-depends` and run `stack build` again. Then import it in your code:

    import Data.Aeson (FromJSON, ToJSON)

You can tell Aeson how to convert your objects to JSON manually, but it's easier to use some fancy GHC features. One of them, called Generics, lets you automatically do things based on the data type. Add these to the top:

    {-# LANGUAGE DeriveGeneric #-}
    import GHC.Generics

Now we can have our data type "derive" Generic

    data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

Then add this below to make any `User` JSON serializable.

    instance ToJSON User
    instance FromJSON User

Now `User` can be encoded or decoded. Let's see if it works. Reload in GHCI and test:

    *Main> :r
    *Main> import Data.Aeson (encode)
    *Main Data.Aeson> encode bob
    "{\"userName\":\"bob\",\"userId\":1}"

Return Users from our API
-------------------------

Let's add a new route `/users` that will return a list of users. We'll use scotty's `json` function instead of `text`. It will automatically call `encode` for us.

    get "/users" $ do
      json allUsers

Let's test it in the browser: [http://localhost:3000/users](http://localhost:3000/users)

    [{"userName":"bob","userId":1},{"userName":"jenny","userId":2}]

Or we can return all users with a given id. First, let's make a pure function that returns true if an id matches. Put this at the top level

    matchesId :: Int -> User -> Bool
    matchesId id user = userId user == id

Now add a new route that uses that function:

    get "/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers)

Test it out: [http://localhost:3000/users/1](http://localhost:3000/users/1)

    [{"userName":"bob","userId":1}]

The complete program
--------------------

Here's what `src/Main.hs` should look like when you're done. Check out the [full source here][source]

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DeriveGeneric #-}

    import Data.Monoid ((<>))
    import Data.Aeson (FromJSON, ToJSON)
    import GHC.Generics
    import Web.Scotty

    data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
    instance ToJSON User
    instance FromJSON User

    bob :: User
    bob = User { userId = 1, userName = "bob" }

    jenny :: User
    jenny = User { userId = 2, userName = "jenny" }

    allUsers :: [User]
    allUsers = [bob, jenny]

    matchesId :: Int -> User -> Bool
    matchesId id user = userId user == id

    main = do
      putStrLn "Starting Server..."
      scotty 3000 $ do
        get "/hello/:name" $ do
            name <- param "name"
            text ("hello " <> name <> "!")

        get "/users" $ do
          json allUsers

        get "/users/:id" $ do
          id <- param "id"
          json (filter matchesId allUsers)

Playing around
--------------

Check out the [Scotty docs][scotty] to see what else you can do. Remember, anything that returns `ScottyM a` can be used in the routes `do` block, and anything that returns an `ActionM a` can be used in the handlers.

Assignment
----------

Create a route that accepts a `User` via `POST`, and prints it back out.

[aeson]: https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html
[scotty]: https://github.com/scotty-web/scotty
[scotty-docs]: https://hackage.haskell.org/package/scotty-0.10.2/docs/Web-Scotty.html
[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[source]: https://github.com/seanhess/practical-haskell/tree/master/04-rest-api

[getting-started]: http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[importing-code]: http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html
[using-monads]: http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
[json-api]: http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html

