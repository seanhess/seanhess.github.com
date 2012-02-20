---
layout: post
title: Learn from Haskell - Functional, Reusable JavaScript 
---

{{ page.title }}
================

Learn You a Haskell: For Great Good?
------------------------------------

For the last couple months I have been [learning][lyah] [Haskell][rwh]. Because there are so many unfamiliar concepts, it feels like learning to program all over again. At [i.TV](http://corp.i.tv), we write a lot of JavaScript ([node.js][node] and front end). While many functional/haskell paradigms don't translate, there are a few techniques that JS can benefit from. 

There are Haskell library functions for *everything*. At first I thought this was just because it was mature, but then I noticed that these functions could be applied to a wider variety of problems than in other languaes. This makes them more useful, as you are less likely to have to write your own solution to a common problem. 

These functions are *composable*[^1]: They are focused on solving one problem without making any assumptions about your code, so you can mix and match the ones you need to solve bigger problems.


Higher Order Functions
----------------------

Many of the most reusable functions in Haskell are [higher order functions][hof] — they either take a function as an argument, or return a function. This makes them inherently flexible. Here's an inflexible function: it counts the number of items in an array that match a value. 

{% highlight javascript %}
// inflexible
function countMatching(array, value) {
    var counted = 0
    for (var i = 0; i < array.length; i++) {
        if (array[i] == value) 
            counted++
    }
    return counted
}

// == 2
countMatching([1,3,3,4,5], 3) 
{% endhighlight %}

It's inflexible because you can only use this function to count the number of items *exactly matching a value*. 

Below is a more flexible version, which takes a function instead of a value. We can use it for any kind of matching, and any kind of object. 

{% highlight javascript %}
// more flexible
function count(array, matching) {
    var counted = 0
    for (var i = 0; i < array.length; i++) {
        if (matching(array[i]))
            counted++
    }
    return counted
}

// == 2, same as first example
count([1,3,3,4,5], function(num) {
    return (num == 3)
})

// == 2, now we can use our functions for ANY kind of items or match test!
count([{name:"bob"}, {name:"henry"}, {name:"jon"}], function(obj) {
    return (obj.name.length < 4)
})
{% endhighlight %} 

Since higher order functions are more flexible, you're less likely to have to write them. Once you write one, you can use it in many different situations. 

Reusable Matching Functions
---------------------------

You probably noticed that `count` is more verbose than `countMatching`. In addition, while `count` is reusable, the matching functions[^2] are not. While this seems fine for these simple cases, it's very likely we'll want more complicated matching functions. These matching functions could be used for all kinds of things, not just counting, so creating or finding them will save us time and bugs in the long run. 

Let's define some reusable matching functions to clean this up. `==` isn't a function. Would it help if we had a function `eq` that did the same thing?

{% highlight javascript %}
function eq(a, b) {
    return (a == b)
}

count([1,3,3,4,5], function(num) {
    return eq(3, num)
})
{% endhighlight %}

We've made a step forward: We are using a library function to match instead of custom code. If `eq` were more complicated, we would test it and use it elsewhere. 

This doesn't help the verbosity, though, because `count` takes a function with one parameter, the item, and `eq` takes two parameters. We still had to define our own anonymous function. Let's try to reduce the verbosity a little bit. 

{% highlight javascript %} 
function makeEq(a) {
    // countMatchingWith wants a function that takes 
    // only 1 argument, just like the one we're returning
    return function(b) { 
        return eq(a, b)
    }
}

// now it's only on one line!
count([1,3,3,4,5], makeEq(3))
{% endhighlight %}

We're generating a function that is compatible with `count` (one argument, the item, and returns true or false). It's as if `count` is calling `eq(3, item)`. We created a function that calls `eq` with the first argument frozen at 3. This is called partial function application.

Partial Application
-------------------

[Partial Function Application][pa] is to create a function that calls another function with some of the arguments pre-set, so that it can be called by something else, like `count`, that expects fewer arguments. We've done this with `makeEq`, but we don't want to create `makeX` versions of all of our functions. Let's come up with a way to do this for any function. 

{% highlight javascript %}
function applyFirst(f, a) {
    return function(b) {
        return f.call(null, a, b)
    }
}

count([1,3,3,4,5], applyFirst(eq, 3))
{% endhighlight %} 

Now we don't need a `makeEq` function. We can use any 2-argument library function the same way. With partial application, defining functional versions of even simple things like `==` makes sense because we can use them in higher-order functions more easily. 

What about functions with more than 2 arguments? This version[^3] lets us apply as many arguments as we want, and the higher order function can add one argument of its own.

{% highlight javascript %}
function apply(f) {
    var args = Array.prototype.slice.call(arguments, 1)
    return function(x) {
        return f.apply(null, args.concat(x))
    }
}

function propertyEquals(propertyName, value, obj) {
    return (obj[propertyName] == value)
}

count([{name:"bob"},{name:"john"}], apply(propertyEquals, "name", "bob")) // == 1
{% endhighlight %}

We applied 2 arguments, "name" and "bob", and count provides the last one to complete the call. Partial function application lets us take generic functions, like `eq`, and use them other generic higher order functions, like `count`, to solve specific problems.

Partial Application with ES5 Map and Filter
-------------------------------------------

There are some great higher order functions built in to [ES5](http://kangax.github.com/es5-compat-table/), and [underscore](http://documentcloud.github.com/underscore/) has many more. Let's look at `filter`, which filters an array, given a matching function. 

{% highlight javascript %}
// this equals [1,3,3]
[1,3,3,4,5].filter(function(num) {
    return (num < 4)
})
{% endhighlight %}

Let's use a reusable matching function `lt` (less than) instead. 

{% highlight javascript %}
function lt(a, b) {
    return (a < b)
}

[1,3,3,4,5].filter(apply(lt, 4))
{% endhighlight %}

It might seem silly to have created `lt` at all, but we can partially apply it to create a concise matching function, and if it were any more complicated we'd benefit from reuse. 

`map` lets you convert an array of things into other things.  

{% highlight javascript %}
var usersById = {"u1":{name:"bob"}, "u2":{name:"john"}}
var user = {name:"sean", friendIds: ["u1", "u2"]}

// == ["bob", "john"]
function friendsNames(usersById, user) {
    return user.friendIds.map(function(id) {
        return usersById[id].name
    })
}
{% endhighlight %}

We can make a reusable mapping function, just like we made reusable matching functions earlier. Let's make one called `lookup`.

{% highlight javascript %}
function lookup(obj, key) {
    return obj[key]
}

// == [{name:"bob"}, {name:"john"}]
function friends(usersById, user) {
    return user.friendIds.map(apply(lookup, usersById))
}
{% endhighlight %}

This is close, but we wanted the names, not the friend objects themselves. If we had a version of lookup with the arguments reversed, we could map over it a second time to get the names. 

{% highlight javascript %}
function lookupFlipped(key, obj) {
    return lookup(obj, key)
}

// == ["bob", "john"]
function friendsNames(usersById, user) {
    return friends(usersById, user)
            .map(apply(lookupFlipped, "name"))
}
{% endhighlight %}

But I don't want to define `lookupFlipped`, that's dumb. Instead, let's make a function that applies arguments to the right instead of the left, so can use `lookup` instead. 

{% highlight javascript %}
function applyr(f) {
    var args = Array.prototype.slice.call(arguments, 1)
    return function(x) {
        return f.apply(null, [x].concat(args))
    }
}

// == ["bob", "john"]
function friendsNames(usersById, user) {
    return friends(usersById, user)
            .map(applyr(lookup, "name")) // we can use normal lookup!
}
{% endhighlight %}

`applyr(lookup, "name")` creates a function to be called with one argument, the object, and returns the object's name. We no longer need to flip anything: we can apply arguments to either side of the function. 

Partial Application requires defining a bunch of functional versions of common things, like `lt`, but that's the point. You can use partially applied `lt` for both `count` and `Array.filter`. They're reusable and composable. 

[Function Composition][fc]
--------------------------

In the previous example, we looped through the array twice, once to get the users, and again to get the names. It would be more efficient to loop through once, and do both mappings at the same time.

{% highlight javascript %}
function friendsNames(usersById, user) {
    return user.friendIds.map(function(id) {
        var friend = lookup(usersById, id)
        return lookup(friend, "name")
    })
}
{% endhighlight %}

We are taking the result of the first lookup, and passing it into the second lookup. [Function composition][fc] means to chain multiple functions into a new, single function, with each step passing its result to the next. 

Let's create a higher-order function to do this for us, then we can rewrite `friendsNames` with a single mapping function. Note that the functions happen in right-to-left order, just as they would if you were writing `f(g(x))`.

{% highlight javascript %}
function compose(f, g) {
    return function(x) {
        return f(g(x))
    }
}

function friendsNames(usersById, user) {
    return user.friendIds.map(applyr(lookup, "name")), compose(apply(lookup, usersById))
}
{% endhighlight %}

This only loops through the array once, and creates a single mapping function exactly like the first example.

We weren't able to use the `friends` function we created, because while it contains the logic for how to get a friend, it also contains the mapping. **The `friends` function is less reusable because it does too much — It's too specific**. What if, instead, we created a `friend` function that mapped only one friend, and a `name` function that returns the name of something?

{% highlight javascript %}
var friend = lookup // lookup happens to have the signature we want. 
var name = applyr(lookup, "name")

function friendsNames(usersById, user) {
    // this line is now more semantic. 
    return user.friends.map(compose(name, apply(friend, usersById)))
}
{% endhighlight %}

Instead of defining a `friends` function, which does both the conversion *and* the iteration, we define `friend`, which does the conversion, and we already have `map` which does the iteration. `friends` is more reusable than `friends` because it is less specific, and can be used in more situations. 

See [here][jfc] for more information on function composition in JavaScript.

Functional and Focused Makes a Clean Codebase
---------------------------------------------

I find myself writing a lot of JavaScript logic from scratch. This isn't just slower than using existing tools, it's more bug-prone and harder to read and maintain. With higher order functions and partial function application, we can create reusable tools that focus on exactly the part of a problem they are trying to solve.  

Over time, instead of a project increasing in complexity and this becomes more and more coupled, we benefit from a growing library of useful tools that can be tested independently of each other, resulting in a healthier, more stable code base. 

[^1]: Composable in the generic sense. This doesn't refer to either function or object composition, just the idea that you work with small tools to build large ones. 

[^2]: "Matching functions" are called [predicates][pred], but I'm trying to avoid introducing new terminology. 

[^3]: See [here][pajmsdn] for more general implementations of `apply`.

[pred]: http://en.wikipedia.org/wiki/Predicate_(mathematical_logic) "Predicate"
[hof]: http://en.wikipedia.org/wiki/Higher-order_function "Higher Order Function"
[composition]: http://en.wikipedia.org/wiki/Object_composition "Object Composition"
[node]: http://nodejs.org/ "Node.js"
[lyah]: http://learnyouahaskell.com/ "Learn You a Haskell"
[rwh]: http://book.realworldhaskell.org/read/ "Real World Haskell"
[pa]: http://en.wikipedia.org/wiki/Partial_application "Partial Application"
[pajmsdn]: http://msdn.microsoft.com/en-us/scriptjunkie/gg575560 "Partial Application in Javascript"
[jfc]: http://javascriptweblog.wordpress.com/2010/04/14/compose-functions-as-building-blocks/ "Compose: functions as building blocks"
[fc]: http://en.wikipedia.org/wiki/Function_composition_(computer_science) "Function Composition"

[Discuss on Hacker News](http://news.ycombinator.com/item?id=3614099)
