---
layout: post
title: Haskell Teaches Javascript — Higher Order Functions and Partial Application
---

{{ page.title }}
================

Learn You a Haskell: For Great Good?
------------------------------------

I have spent the last couple months learning Haskell. Because there are so many unfamiliar concepts, it feels like learning to program all over again. At i.TV, we write a lot of JavaScript (node.js and front end). While many functional/haskell paradigms don't translate, there are a few techniques that JS can benefit from. 

There are Haskell library functions for *everything*. At first I thought this was just because it was mature, but then I noticed that these functions could be applied to a wider variety of problems than in other languaes. This makes them more useful, as you are less likely to have to write your own solution to a common problem. 

These functions are [composable][BELOW]: They are focused on solving one problem without making any assumptions about your code, so you can mix and match the ones you need to solve bigger problems.


Higher Order Functions
----------------------

Many of most reusable functions in Haskell are "higher order functions" — they either take a function as an argument, or return a function. This makes them inherently flexible.

Here's an inflexible function: it counts the number of items in an array that match a value. 

{% highlight javascript %}
function countMatching(array, value) {
    var counted = 0
    for (var i = 0; i < array.length; i++) {
        var item = array[i]
        if (item == value) 
            counted++
    }
    return counted
}

// == 2
countMatching([1,3,3,4,5], 3) 
{% endhighlight %}

It's inflexible because you can only use this function to count the number of items *exactly matching a value*. Here's a more flexible version, which takes a function instead of a value. We can use it for any kind of matching, and any kind of object. 

{% highlight javascript %}
function count(array, matching) {
    var counted = 0
    for (var i = 0; i < array.length; i++) {
        var item = array[i]
        if (matching(item))
            counted++
    }
    return counted
}

// == 2, same as first example
count([1,3,3,4,5], function(num) {
    return (num == 3)
})

// == 2, now we can use our functions for ANY kind of items or match test!
count([{name:"bob"}, {name:"henry"}, {name:"bob"}], function(obj) {
    return (obj.name.length < 4)
})
{% endhighlight %} 

Since higher order functions are more flexible, you're less likely to have to write them. You can create or find libraries of useful functions and use them in many situations. 

Flexible and Concise
--------------------

You probably noticed that `count` is more verbose, even when we do an exact match. In addition, while `count` is really reusable, the matching functions are not. While this seems fine for these simple cases, it's very likely we'll want more complicated matching functions. These matching functions could be used for all kinds of things, not just counting, so creating or finding them will save us time and bugs in the long run. 

Let's define some reusable matching functions to clean this up. `==` isn't a function. Would it help if we had a function `eq` that did the same thing?

{% highlight javascript %}
function eq(a, b) {
    return (a == b)
}

count([1,3,3,4,5], function(num) {
    return eq(3, num)
})
{% endhighlight %}

We've made a step forward: We are using a library function to match instead of custom code. If this were complicated, we could test `eq` and use it elsewhere. 

That doesn't help the verbosity, because count takes a function with one parameter, the item, and eq takes two parameters. We still had to define our own anonymous function. We have made a step forward though: we are using a library function for the matching instead of custom code. 

Let's try to reduce the verbosity a little bit. 

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

We're generating a function that is compatible with `count` (one argument, the item, and returns true or false). It's as if `count` is calling `eq` with (3, item). We created a function that calls `eq` with the first argument frozen at 3. This is called partial function application.

Partial Application
-------------------

Partial Function Application is to create a function that calls another function with some of the arguments pre-set, so that it can be called by something else, like `count`, that expects fewer arguments. We've done this with `makeEq`, but we don't want to create `makeX` versions of all of our functions

Let's come up with a way to do this for any function. 

{% highlight javascript %}
function applyFirst(f, a) {
    return function(b) {
        f.call(null, a, b)
    }
}

count([1,3,3,4,5], applyFirst(eq, 3))
{% endhighlight %} 

Now we don't need a `makeEq` function! We can use any 2-argument library function the same way. What about functions with more than 2 arguments? This version lets us apply as many arguments as we want, and the higher order function can add as many as it wants later. 

{% highlight javascript %}
function apply(f) {
    // arguments isn't an array, so it doesn't have slice
    var appliedArgs = Array.prototype.slice.call(arguments, 1)
    return function() {
        var args = appliedArgs.concat(arguments)
        f.apply(null, args)
    }
}

function propertyEquals(propertyName, value, obj) {
    return (obj[propertyName] == value)
}

count([{name:"bob"},{name:"john"}], apply(propertyEquals, "name", "bob"))
{% endhighlight %}

We applied 2 arguments, and count provides the last one to complete the call. Partial function application lets us take generic functions, like `eq`, and use them other generic higher order functions, like `count`, to solve specific problems. 

Partial Application with ES5 Map and Filter
---------------------------------------

There are some great higher order functions built in to ES5, and underscore has many more. `map`, `filter`, and `reduce` are a some of the best.

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

It might seem silly to use `lt` and `eq`, but in addition to being more concise, for anything more complicated you get to test and reuse the matching function.

Map lets you convert an array of things into other things.  

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

We can make a reusable mapping function, just like we made a reusable matching function earlier. Let's make one called `lookup`.

{% highlight javascript %}
function lookup(obj, key) {
    return obj[key]
}

// == [{name:"bob"}, {name:"john"}]
function friends(usersById, user) {
    return user.friendIds.map(apply(lookup, usersById))
}
{% endhighlight %}

This is close, but we wanted the names, not the friend objects themselves. If we had a version of lookup with the arguments reversed, we could map over it a second time. 

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

But I don't want to define lookupFlipped, that's dumb. Instead, let's make a function that applies arguments to the right instead of the left. 

{% highlight javascript %}
function applyr(f) {
    var appliedArgs = Array.prototype.slice.call(arguments, 1)
    return function() {
        var args = Array.prototype.concat.call(arguments, appliedArgs)
        f.apply(null, args)
    }
}

// == ["bob", "john"]
function friendsNames(usersById, user) {
    return friends(usersById, user)
            .map(applyr(lookup, "name")) // we can use normal lookup!
}
{% endhighlight %}

`applyr(lookup, "name")` creates a function to be called with one argument, the object, and returns the object's name. We no longer need to flip anything: we can apply arguments to either side of the function. 

This approach requires defining a bunch of functional versions of common things, like `lt`, but that's the point. You can use partially applied `lt` for both `count` and `Array.filter`. They're reusable and composable. 

This example could be better solved with [function composition](http://google.com), but this article is too long already. 


A More Complicated Example: SortBy
----------------------------------

















Higher Order Functions 
----------------------

In a language like JavaScript, you can reuse code in new ways with Higher Order Functions, or functions that take functions as arguments. You've probably used them with jQuery,  
















































Learned Me Some Haskell For Great Good!
---------------------------------------

I'd been using the word "functional" for months before I decided to learn what it really meant. After looking around [1](stack overflow question?) I decided to learn Haskell, because I was told it takes the functional paradigm the furthest. 

It was a frustrating experience at first, not just because I couldn't get my code to compile, but because it was like I was learning to program all over again. While I don't know every language, I thought I understood most of the core concepts in languages. Haskell proved that there's much more to learn. 

While I struggled with new the concepts, I quickly realized that library functions in Haskell are more useful than in other languages. I was less likely to need to write my own solution to common problems, not because the tools and libraries cover every situation, but because I could apply them to a wider variety of problems. 

These tools were more useful because they were [composable][BELOW]: You can put several of them together to form another tool. One thing that makes composition easier is [Partial Function Application][BELOW]. 

At i.TV, we use Javascript both on our servers and for our web clients. There are some things from Haskell that simply don't apply to JavaScript, like the type system, but JS code can become more reusable with a focus on composition and partial application. 

Composition: Legos instead of a Swiss Army Knife
-----------------------------------------------------

There are many ways to make code reusable. One of the most common is to put useful functionality into a superclass. Another is to create many small, specific tools. 

Putting functionality into a baseclass has its benefits. There is less boilerplate. You can do a lot with a single function call. The disadvantage is that it can become inflexible. What if your class doesn't inherit from the class that has the functionality you need?

This approach is like a Swiss Army Knife - the knife has many useful features, and you can accomplish many tasks with it. However, the base class has to anticipate everything you could possibly want to do.  

Composition is more like legos: You build the tool you need from basic building blocks. You have to write a little more boilerplate code to snap things together (compose them), but it can lead to better design. Partial application allows us to take basic building blocks, and cleanly make them more specific, making composition easier and the tools more reusable. 

Partial Application Makes a Generic Function Specific
-----------------------------------------------------

In Haskell, you can call any function with fewer arguments than it needs. This returns a second function that you can call with the remaining arguments. This is called partial application. 

{% highlight haskell %}
let add a b = a + b
let add2 = add 2
add2 1 == add 2 1
{% endhighlight %}

It is more verbose in javascript

{% highlight javascript %}
function add(a, b) { return a + b }
function add2(b) { return add(2, b) }
add2(1) == add(2, 1) 
{% endhighlight %}

So partial function application lets you take a generic function, and make it specific to your needs. This in turns facilitates composition, as you adapt many small, generic functions to your needs. 

Example: Repeating Yourself
---------------------------

In JavaScript, you might write a function like this:

{% highlight javascript %}
function mapItemsByType(someItems, someMoreItems) {
    var itemsByType = {} // {type: [array of stuff]}

    function mapItem(item) {
        if (!itemsByType)
            itemsByType[item.type] = []
        itemsByType[item.type].push(item)
    }

    for (var i = 0; i < someItems.length; i++) {
        mapItem(someItems[i])
    }   

    for (var i = 0; i < someMoreItems.length; i++) {
        mapItem(someMoreItems[i])
    }   

    return itemsByType
}
{% endhighlight %}

In this example, we created a function mapItem inline to avoid having to repeat the logic that lazily creates the items array for the type. You could make a function that does the same thing but passes the itemsByType in. Then it can be outside the main function, and becomes reusable. 

{% highlight javascript %}
function mapByType(itemsByType, item) {
    ... // same
}

function mapItemsByType(someItems, someMoreItems) {
    var itemsByType = {}
    ... 
    mapByType(itemsByType, someItems[i])
    ...
}
{% endhighlight %}

Having to pass itemsByType in here isn't so bad, but in other cases you might have a lot of variables to pass in. Instead of passing it in each time, we can partially apply `mapByType`. We don't have to repeat the parameter each time we call the function, and our main function can use the mapByType library function instead of defining it itself. 

{% highlight javascript %}
function mapItemsByType(someItems, someMoreItems) {
    var itemsByType = {}
    function mapItem(item) {
        return mapByType(itemsByType, item)
    }
    ...
    mapItem(someItems[i]) // we can leave off the 
    ...
}

{% endhighlight %}

Making Partial Application Cleaner
----------------------------------

We can make this cleaner. Let's define a function `partial` that will partially apply for us: 

{% highlight javascript %}
function partial(f) { // arguments...
    var args = Array.prototype.slice.call(arguments, 1) // remove f
    f.apply(null, args)
}

function mapItemsByType(someItems, someMoreItems) {
    var itemsByType = {}
    var mapItem = partial(mapByType, itemsByType)
    ...
}
{% endhighlight %} 

Much cleaner! Now we don't have to define a higher-order function every time we want to partially apply something. We'll use this syntax from now on.

Example: Sorting
----------------

Have you ever wanted to sort an array of objects by one of their fields? `Array.sort()` will compare the objects themselves. Here's how you would get it to work normally.  

{% highlight javascript %}
function sortItemsByName(items) {
    return items.sort(function(a, b) {
        if (a.name < b.name) return -1
        if (a.name > b.name) return 1
        return 0
    })
}
{% endhighlight %}

We have the same problem. This is application specific code. It's not to hard to make this take a parameter. 

{% highlight javascript %}
function sortBy(p, items) {
    return items.sort(function(a, b) {
        if (a[p] < b[p]) return -1
        if (a[p] > b[p]) return 1
        return 0
    })
}

function exampleUse() {
    var items = getSomeItems()
    return sortBy("name", items)
}
{% endhighlight %}

Ok, great, no problem, it's generic. But wait

{% highlight javascript %}
function compareProperty(p, a, b) {
    if (a[p] < b[p]) return -1
    if (a[p] > b[p]) return 1
    return 0
}

function exampleUse() {
    var items = getSomeItems()
    return items.sort(partial(compareProperty, "name"))
}
{% endhighlight %}

We didn't need to define a new SORT function at all. We only needed a new COMPARE function. We could use the same compare function to enable all kinds of different sorting functionality. We get to use the built-in, default sort logic. We only specify the part we want to change, the compare. 

{% highlight javascript %}

function isFirst(compare, a, b) {
    return (compare(a, b) > -1)
}

function example() {
    var a = ...
    var b = ...

    var compareByName = partial(compareProperty, "name")

    if (isFirst(compareByName, a, b)) {
        // do something special
    }
}

{% endhighlight %}

We got to use compareProperty for something other than sorting! We've written tests for it at this point, so we trust it. By defining the functionality at the lowest level, the comparator, rather than definition a sort function, we can re-use it with sort logic. 

{% highlight javascript %}

function compare(a, b) {
    if (a < b) return -1
    if (a > b) return 1
    return 0
}

function compareProperty(p, a, b) {
    return compare(a[p], b[p])
}

{% endhighlight %}

Do you see how we are starting to re-use the most basic building blocks? We don't have to redefine exiting logic, EVER, because our functions are as general as possible. We can make them more specific by applying them partially. 


[pa]: http://en.wikipedia.org/wiki/Partial_application "Partial Application"

[pajmsdn]: http://msdn.microsoft.com/en-us/scriptjunkie/gg575560 "Partial Application in Javascript"





Can I bring this together somehow? Perhaps one extended example? Start with some gnarly code, and improve it.
    - mapping
    - sorting
    - calling the same method over and over again. 

    -- blog posts?
    -- facebook users, they have lots of friends. You have to get them from the API 
        -- you have to get each one from the api, then sort them by the number of friends?
        -- you have to fetch all their friends, then sort them into the right one?
    
    function that converts responses from the server into user objects. 
        -- but it requires looking up information from a hash?
        -- sorting them all by number of friends

    haskell is at its most powerful with type classes, when you take an object you know conforms, and so long as you implement a small bit, how it fits into the puzzle, it'll work with the rest. 
    but I'm talking about partial application here, no? 
    so what's a good use case for partial application?
    I guess I don't really know. 
        - what if you had to sort them by the list of friends? 
        - don't get all into map and stuff. 
    map is cool because: it shows you should make the lowest-level

    sort by is a better example, because it shows how you can start athe lowest level. 
        BUT it has to be something you want to pass around, and reuse. Otherwise, you might as well just write the dang thing. 

    So why would you need to pass around a sort function?
        - we needed to do a complicated operation on a list of stuff (sort each item in a list?)
        - the operation requires a common parameter for the list, the type/kind/something. 

    What if you had a big list of users, with references by friend id, and you needed to convert it into a big old mapped list. Each user needs an array of friends... Then you need to sort by who has the most friends at the end? 

    users = [{name: "bob", id: 3, friends: [1,4}]
    users = [{name: "bob", id: 3}]
    users = [{name: "bob", id: 3}]

    so you need to convert them to friends: [{other friend}] and make a big old tree. 

    you need to search through each one. You don't have the correct friend object yet (because you need to convert it to the new objects). THEN each one needs to link to all its friends, and have access to the (read-only) map of friends so they can get theirs out. 

    nawww... 

    something with a blog post? 

    Task manager :)
    


    You have a bunch of tasks, they need to be sorted by priority
        - but the priority is a complex calculation

    1. map over a collection
    2. each item, need to calculate something, which depends on something external

    but you start out with an array of things, which don't have a complex property
    function needs to return an array of somethings, sorted by complex property

Calling 

Examples 
--------

?. Calling the same method over and over with the same parameters
2. SortBy
3. Map, with an argument

4. Don't create a function that creates

PARTIAL APPLICATION
-------------------
Most useful when


0. Talk aobut higher order functions. A function that needs to do something variable ßomewhere in its execution. Like an animation function? Weird iterator?
    - or just talk about iteration? 
    - when you want to abstract out some common logic, sometimes its the only way to go
        - call function x when you find something
        - do some stuff, call action with stuff, use results

1. You need to call a higher-order function. A function that will call your function. 
    - map, select, filter, etc
    - callbacks? (generate callbacks, sure)
    - other examples? callbacK/

2. You want to hold state. You need to call X later with local variables
    - config stuff! call an API with a whole bunch of crap
    - happens in async code: you want to process data in the same way
        - what if you had to get data from a variety of sources, and add them all to the same array
        - in the same way. Yeah, like a nested array thing (map them)

    Guide Data: You have events. Map by channel and by event. Use a different model? Or use something similar. And each one needs to be sorted at the end? Crazy :) In other words, we have a function that operates on the same little thing every time. It's not going to save you many characters to do it this way. 

    function getEvents() {
        var map = {}

        firstEvents(function(events) {
            events = convertEvents(events)
            addEvents(events)
        })

        function addEvents(events) {
            for (var i = 0; i < events.length; i++) {
                if (!map[event.user]) map[event.user] = []
                map[event.user].push(event)
            }
        }
    }

An extended example would be great. It's not so great there. What if there's something different you have to do for each one, like convert it differently? You're getting events from different places, and need to convert them differently. You can map them beforehand. 

Well, it really IS about higher order functions. Show how they can be useful. Then show how/why you need partial application BECAUSE of them. 

But it IS useful to be able to include state in them. I just can't think of an example. 
    - logging a bunch of stuff out in a complicated function. 

Maybe I need to have a better handle on the more foundational stuff first. This is dumb. 

HIGHER ORDER FUNCTIONS - i don't have a good grasp on when they're useful. 
    applyTwice f (f x)
    zipWith applies the functions between elements
    takeWhile

    the useful ones are already there. Well, it makes any specific function general! Higher order functions do this. 

    map, filter, reduce 
    sort!
    slice, split

    function animateStuff() {
        
    }

    // 1 // visibility = visible (immediate)
    // 2 // callbacks! Happen all the time. 

    .animateStuff {
        transition: 500ms opacity ease;
    }

    event pattern. Every time x happens, do something with it. 
        it can happen ALL OVER the place. 


I need REAL experience doing this. I don't know enopugh. 
So take one of the exampels you KNOW works, and just write an article about that. 
    map is a cool function. You might very well need partial application for it.  

Just a SIMPLE higher order example that is cool. 
    here's one: bind() takes a function and makes it keep its this pointer

    function bind(f, obj) {
        return function() {
            f.apply(obj, arguments)
        }
    }

    // can't pass arguments through an event listener!
    addEventListener(f, obj) 

    // can make something generic. 

    // looping is generic
    // sorting. take something specific, and make a generic version of it. 


    // write about what you know. 
        // 1 // higher order functions are useful (map, filter), because they are general and composable
        // 2 // partial application makes them powerful
        // 3 // define a sortBy function



# 1. Learned me some haskell
#     - it's HARD and very different from OO
#     - struck by how reusable the code was. 
#     - specifically, how composable the code was << the reason why it is so reusable. 
#     - easy to organize the code. only one decision: name + namespace
# 2. Composability is important
#     - why is it important? Because you don't have to make tools for everything, don't have to clutter base classes, focus.
#     - legos vs prefab components (go manufacture another piece)
#     - composability = reusability via building blocks (legos)
#     - inheritance = reusability via well-designed product (swiss army knife)
#     - fewer tools to remember. Just the basics. (lego block)
#     - focus on building SMALL reusable pieces, not LARGE ones
# 2. Use node.js at work, 
# 3. Partial Application - Why?
#     - composable code
#     - it's the KING of composability. You don't need separate functions for everything
# 3. Partial Application in Haskell
# 4. Partial Application in Javascript
#     - alternative to closures
#     - cleans up other functional paradigms, like map/reduce, etc
#     - replaces higher-order functions: function() { return function() {} }
#     - .partial, or partial(f, args...)
# 5. Examples Where it is useful
#     - calling the same method over and over with the same parameters :: 
#         - a big function, operates on any number of scope variables
# 
#     - higher order functions :: 
#         - sorting? - sortby function
#     
#     - composition: making multiple low-level tools. (inspecific, general)
#         functions that operate on an object, rather than instance methods?
#         an example of something specific, that you can generalize and combine with something else
#         mapping - mapping only takes one parameter
#             - yeah! a reusable mapping function
#         normally you would just write a one-off mapping function
#         combine with your sort and mapping function and show how they can be used for different things
# 
#         map an array by a key in the array
# 
#     -- mapping, a function that converts stuff via a map
#         - version that loops through and converts all the objects
#         - don't even MAKE the version that takes an array. 
#         - instead, just make the function that converts ONE
#         - what if you need a parameter available in your current block?
#         - partial application!
# 
#     - async flow stuff: 
#         
# 
# -- Maybe I should just do it all at once?
# -- talk about map/reduce, etc... 
# -- reference haskell in the title?
# 
# -- creating reusable functions, with only a few parameters. 
# -- I don't need haskell in the title - 
# -- there's more than enough detail to go in to with partial application 

