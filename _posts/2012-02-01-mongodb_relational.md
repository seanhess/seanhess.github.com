---
layout: post
title: You Only Wish MongoDB Wasn't Relational
---

{{ page.title }}
================


MongoDB = Get Stuff Done
------------------------

**Update:** Changed blog example to use a normal belongs-to relationship.

**Update:** Added examples of $slice and $elemMatch to show why they don't work

When choosing the stack for our TV guide service, we became interested in [NoSQL][nosql] dbs because we anticipated needing to scale horizontally. We evaluated several and settled on [MongoDB][mongodb]. The main reason was that MongoDB got out of the way and let us get work done. You can read a little more about our production setup [here](http://seanhess.posterous.com/surviving-a-production-launch-with-nodejs-and).  

So when you read that MongoDB is a [document store][doc], you might get the wonderful idea to store your relationships in a big document. Since mongo lets you [reach into objects](http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29), you can query against them, right?  

Several times, we've excitedly begun a schema this way, only to be forced to pull the nested documents out into their own collection. I'll show you why, and why it's not a big deal.  

Let's make a blog!
------------------

Here's an example. You just discovered that Mongo lets you store nested documents. You set out to make a blog.

~~~ javascript
db.posts.save({ title: "My First Blog Post"
              , content: "Here is my super long post ..." 
              , created: 1328118062598
              })
~~~

What's a blog without comments? Hey, mongo is awesome! Let's nest the comments. Multiple collections are for RDBMS dinoasaurs. 

~~~ javascript
{ title: "My First Blog Post"
, content: "Here is my super long post ..." 
, comments: [ { text: "This post sucks!"
              , name: "seanhess"
              , created: 1328118162000 }
            , { text: "I know! I wish it were longer"
              , name: "bob"
              , created: 1328118262000 } 
            ] 
}
~~~

This works nicely with the key/value idea that you should [denormalize][d] your data. You can avoid joins if all the data you need for a call is in one document. It's easy to treat these nested documents as if they were top-level. As long as your blog post displays the post and all comments, this works great:
 
~~~ javascript
function postWithAllComments(id) {
    return db.posts.findOne({_id: id})
}

function addComment(postId, comment) {
    comment.created = Date.now()
    db.posts.update({_id: postId}, {$push: {'comments': comment}})
}
~~~

When Nested Documents Become a Problem
--------------------------------------

The minute you need just a little control over querying against those comments, you're stuck. While you can use [`$slice`][slice] to limit comments to a certain number/offset, what if you want to display all comments made by a certain user? You're first tempted to try this:

~~~ javascript
// bad idea
function commentsByUser(username) {
    // $slice can't help you here
    return db.posts.find({"comments.name": username}, {comments: 1}).toArray()
}
~~~

Which seems to work, except it returns *all* the comments for each post they commented on. There's no way to get only the matched comment back. If there are 1000 comments per post, and the user commented on 3 posts, you'll have to wade through 3000 comments. 

Another thing that I need to do a lot is get a range of documents. Perhaps we want to see all comments made yesterday. You have the same problem: you get all the comments for each post. 

~~~ javascript
// bad idea
function commentsBetweenTimes(start, end) {
    return db.posts.find( {"comments.created": {$gte: start, $lt: end}}
                        , {comments: 1}
                        ).toArray()
}
~~~

Collections are Cheap
---------------------

It turns out it isn't hard to put the comments in their own collection. If you restructure your posts to look like this, everything falls into place:

~~~ javascript
// db.posts
{ _id: "4f297e550b3e6d9e2b7aa58e"
, title: "My First Blog Post"
, content: "Here is my super long post ..." 
}

// db.comments
{ postId: "4f297e550b3e6d9e2b7aa58e"
, text: "This post sucks!"
, name: "seanhess"
, created: 1328118162000 }
~~~

Your client doesn't have to be aware of the change, just add the comments with a second query. Now the comment-specific queries return only comments.

~~~ javascript
function postWithAllComments(id) {
    var post = db.posts.findOne({_id: id})
    post.comments = db.comments.find({postId: id}).toArray()
    return post
}

function addComment(postId, comment) {
    comment.created = Date.now()
    comment.postId = postId
    db.comments.save(comment)
}

function commentsByUser(username) {
    return db.comments.find({"name": username}).toArray()
}

function commentsBetweenTimes(start, end) {
    return db.comments.find({"created": {$gte: start, $lt: end}).toArray()
}
~~~

While our postWithAllComments function requires 2 queries, it's very fast. Everything comes out of memory, and from an index. The other functions are as fast or faster and are easy to write. 

Many-to-Many Relationships
--------------------------

It's impossible to model many-to-many relationships without storing them in a separate collection, unless you copy the data to every document. This is fine for tags, but can be unrealistic if your data set is large. It also means that like our blog you can't query the nested documents themselves. Here's a post with many-to-many tags.  

~~~ javascript
db.posts.save({ title: "My Post", tags: ["awesome", "incredible"] })
db.posts.find({ tags: "awesome" }) // works!
~~~

Let's say you want to record users that are attending events. You could store this data a variety of ways, but it's very likely that you'll want to get both the users attending an event, and the events a user is attending. It doesn't make sense to put either users under event, or events under users. 

Even though you're forced to be relational, a HUGE advantage over SQL is that you don't have to have a separate joining collection. We can store the relationships inside the documents. Put the relationship on both if you want to query both directions. 

~~~ javascript
var party = { _id: "chessparty"
            , name: "Chess Party!"
            , attendees: ["seanhess", "bob"] }
var user = { _id: "seanhess", name: "Sean Hess", events: ["chessparty"]}
db.events.save(party)
db.users.save(user)

db.events.find({_id: {$in: user.events}}) // events for user
db.users.find({_id: {$in: party.attendees}}) // users for event
~~~


[Denormalization][d] might be important
-----------------------------------------------------

So, that said, sometimes you need your queries to be really fast. If your list of users only needs to show their names, you can nest their names along with their ids, allowing you to display your page without a join. You're not *storing* the users within events, your *copying* some of the fields you need to events, so you can skip the [$in][in] query. This is called [denormalization][d], and the [usual warnings about early optimization](/2011/12/15/optimization_is_like_firing_clay.html) apply.  

~~~ javascript
{ _id: "chessparty"
, name: "Chess Party!"
, attendees: [ {_id:"seanhess", name:"Sean Hess"}
             , {_id:"bob", name:"Bob Somebody"} ] 
}
~~~

Don't fight the Mongo
---------------------
MongoDB just lets you get stuff done. Don't become a [NoSQL][nosql] or [Document Store][doc] purist, just write code that works. It's the mongo way. It's easy to store relationships in a separate collection, and the joins are pretty cheap if you don't split up your data too much. Don't be overly tempted to store everything in a nested document, because at least in my experience, you end up needing to query against them sooner rather than later. 

What about X?
-------------

**[$slice][slice]** doesn't solve the problem, because you can only give it offsets, which is great for paging, but doesn't return the comments you matched on. 

~~~ javascript
db.posts.save({comments:[{num:1, name:"sean"}, {num:2, name:"bob"}]})
db.posts.find()
// { "comments" : [ { "num" : 1, "name" : "sean" }, { "num" : 2, "name" : "bob" } ] }
db.posts.find({"comments.name":"bob"}, {comments:{$slice: 1}})
// { "comments" : [ { "num" : 1, "name" : "sean" } ] }
// I want the 2nd comment, not this one ^^
~~~

**[$elemMatch][elem]** doesn't work either, it was intended to help you match nested documents more accurately, but still doesn't give you only the matched comments 

~~~ javascript
db.posts.find({comments: {$elemMatch: {name:"bob"}}})
// { "comments" : [ { "num" : 1, "name" : "sean" }, { "num" : 2, "name" : "bob" } ] }
~~~


[Discuss on Hacker News](http://news.ycombinator.com/item?id=3539385)

[d]: http://en.wikipedia.org/wiki/Denormalization "Denormalization"
[in]: http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24in "MongoDB Advanced Queries - $in"
[doc]: http://en.wikipedia.org/wiki/Document-oriented_database "Document Oriented Database"
[nosql]: http://en.wikipedia.org/wiki/NoSQL "NoSQL"
[mongodb]: http://www.mongodb.org/ "MongoDB"
[slice]: http://www.mongodb.org/display/DOCS/Retrieving+a+Subset+of+Fields#RetrievingaSubsetofFields-RetrievingaSubrangeofArrayElements "Slice"
[elem]: http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29#DotNotation%28ReachingintoObjects%29-Matchingwith%24elemMatch "elemMatch"
