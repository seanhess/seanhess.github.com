---
layout: post
title: Swift - why annoying is good
---

{{ page.title }}
================

Last weekend I attented [Startup Weekend][startupweekend], and I finally got a chance to try [Swift][swift]. We made a simple app called [Wolf Pack][wolfpack] for parents who want to let their kids run around the neighborhood like savages.

<center>
<a href="https://github.com/seanhess/wolfpack-ios">
<img src="/images/wolfpack.png" height="400">
</a>
</center>

Swift is awesome, but I occasionally found myself butting heads with the type system. [I'm a huge fan of type systems](https://github.com/seanhess/angularjs-typescript) so why was Swift getting in my way?

Types vs Convenience: An eternal war
------------------------------------

This isn't the first time that strict typing has slowed me down. A year ago at a [Startup Weekend][startupweekend] I boldly tried to get over my [Haskell][haskell]-novice-coder's block. I joined a team with simple requirements and tried making a REST api. After making about a hundred of these in [Node.JS][nodejs], and some good exposure to Haskell, it should have been easy. 

[I got it printing data from the database](https://github.com/seanhess/spoticka/blob/04fbba81b0b2be85ed42fd553dfaaa2ce0868310/Api.hs#L31), but my pace started to slow right away. Things I never thought about in node required my full attention. This was especially true when it came to handling JSON and the database: the edges of my program. 

With node you can just [take the client's `POST` and throw it in the database](https://github.com/seanhess/spoticka/blob/master/server.js#L82). You can take the result of a database query and dump it out. Easy. In Haskell I found myself spending lots of time writing [code that translated JSON data to my object, to the database, and back again](https://github.com/seanhess/spoticka/blob/04fbba81b0b2be85ed42fd553dfaaa2ce0868310/Spoticka/User.hs#L13). 

> "This is supposed to be automatic! I shouldn't have to write this code at all!"

I gave up after the first day.

Swift is like Haskell
---------------------

When I saw the [Swift][swift] spec, I was excited to see how many ideas they had incorporated from [Haskell][haskell]. Swift has a much stricter type system than Objective-C. One new feature is [Optional Typing](https://developer.apple.com/library/prerelease/mac/documentation/Swift/Conceptual/Swift_Programming_Language/Types.html#//apple_ref/doc/uid/TP40014097-CH31-XID_1109). You can decide whether a variable is ever allowed to be null. To specify this, you put a question mark after the variable declaration. 

    // user can be nil
    var user:User? 
    user = nil
    user = User("Bob")

A variable without the question mark cannot ever be null. You have to provide a value during initialization

    var user:User = User("Bob")

This is only one simple example of many ways in which Swift is more strict than Objective-C

Strict is annoying
------------------

Having to deal with optional types in swift is annoying. If you're used to the old way of doing things, it seems like a step backwards. The following won't compile:

    // No! user doesn't have a value!
    var user:User
    userLabel.text = user.firstName 

The first thing you might realize is that you can code like a "normal" person if you throw in gratuitous punctuation. [Question marks](https://developer.apple.com/library/prerelease/mac/documentation/Swift/Conceptual/Swift_Programming_Language/Types.html#//apple_ref/doc/uid/TP40014097-CH31-XID_1109) make it work the old way:

    // compiles and the label is blank
    var user:User?
    userLabel.text = user?.firstName

Or use [exclamation points](https://developer.apple.com/library/prerelease/mac/documentation/Swift/Conceptual/Swift_Programming_Language/Types.html#//apple_ref/doc/uid/TP40014097-CH31-XID_1112) to pretend it isn't null

    // also compiles, but will crash on the 2nd line.
    var user:User!
    userLabel.text = user.firstName

Why doesn't the program just let me code? The second example will crash if it runs! In what universe is this better than just leaving the field blank if user is nil?

Data Integrity: Not having to worry
----------------------------------

Prior to swift, you had two ways to handle this kind of situation.

You could be careful: make sure your data is all correct going in, check for nils everywhere your program might break, and leave comments with your assumptions. 

Or you could be lazy: just assume that everything is correct, and code as if it is. 

The only thing that Swift's strict type system does is enforce your choices. You can make a type optional, and deal with the chance that it might be nil every time you use it, or you can mark it non-optional and force whoever sets the variable to get it right. 

With Swift, **your type system is enforcing your assumptions**. This is a powerful idea. You don't have to leave a comment. You don't have to update your wiki. It's a part of the code itself.

An exercise in Laziness: Back to JSON
-------------------------------------


In Wolf Pack, we wanted to sync our Core Data store with the REST API. This meant we had to call the API, parse each JSON object into an `NSManagedObject`, and save them. Here's some of the relevant code from [our User model](https://github.com/seanhess/wolfpack-ios/blob/master/Wolf%20Pack/Wolf%20Pack/MOUserExtension.swift#L41).

    class func syncREST() {
        let url = NSURL(string: "http://wolfpack-api.herokuapp.com/users")
        loadRESTObjects(url) { json in
            let id = json["_id"].string!
            var user = self.fetchOrCreate(id)
            user.updateFromJSON(json)
        }
    }

And below is where we map the JSON to the fields. This is almost the most compact way to express this: Take the `firstName` field from the JSON and set it to our `firstName` property. Throw in some gratuitous punctuation to get it to compile:
    
    func updateFromJSON(json:JSONValue) {
        self.firstName = json["firstName"].string!
        self.imageUrl = json["imageUrl"].string!
        // etc
    }

But what this code also says is: "Crash if you don't find `firstName` in the JSON object". Is this what we really want? [^1]

This isn't a theoretical question. This project requires an `imageUrl` when we go to [display an avatar image](https://github.com/seanhess/wolfpack-ios/blob/master/Wolf%20Pack/Wolf%20Pack/MainViewController.swift#L59), or it crashes. The API didn't guarantee an `imageUrl` (the back-end guys were adding the images one at a time).

**By cheating with the exclamation points, we've swept the decision of how to handle bad data under the rug.** It's like pretending a house is clean by throwing a blanket over everything. Saving this kind of complexity for later is Technical Debt: it's what makes you afraid to edit a code base later. 

Here's what we really ended up wanting in this case: if `imageUrl` is missing, set it to a default:

    if let value = json["imageUrl"].string {
        self.imageUrl = value
    } else {
        self.imageUrl = "http://example.com/empty-avatar.png"
    }

Swift encourages you to make these kinds of decisions up front. 

Put a fence around your code
-----------------------------

We are still left with the decision of where to be strict: should we mark types as Optional or force them to be clean earlier in the process? Should we map our data to strictly typed classes or should we use Dictionaries throughout our program?

In the medium/long-term, it's best to deal with unknowns as early as possible. Once you have everything nicely typed, knowing you've handled missing fields, etc, the rest of your code gets much easier to write. 

Think of putting up a fence at the boundaries of your code: enforce the rules when you send or read JSON, or when you get input from a user. That way you get it over with and never have to check again. It makes everything "inside the fence" easy to understand and easy to trust. 

Embrace the strictness
----------------------

So, coming back to that API, it's more complicated than "Node is easier to work with than Haskell". Node encourages me to take a very complex and error-prone process (JSON parsing), and sweep it under the rug for another day. It trades long-term maintenance for short-term gains.

Trusting POST data enough to save it straight into your database is crazy. Clients can put anything they want in that object. Sometimes crazy assumptions are ok. If you're coding a prototype over the weekend, you don't need to worry about tomorrow. **But usually we shouldn't be optimizing for the first week of a project, because it only lasts a week.** The majority of projects take months or years. 

Strictness forces us to pay a price up front to minimize complexity in the future. That's why it's worth it. Type systems do this for us, and the more expressive and strict a type system is, the more it can help you. 

[^1]: Using `!` isn't always bad. Sometimes you want to make assumptions about your code. For example, I pretty much always mark `IBOutlet` variables as `!`, because if they aren't set I probably forgot to attach them, and *I want the program to crash*. But this isn't cheating, it's the intended behavior. 




[wolfpack]: https://github.com/seanhess/wolfpack-ios
[swift]: https://developer.apple.com/swift/
[nodejs]: http://nodejs.org/
[haskell]: haskell.org
[itv]: http://i.tv
[startupweekend]: http://startupweekend.org/