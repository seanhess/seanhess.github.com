---
layout: post
title: You Only Wish MongoDB Wasn't Relational
---

{{ page.title }}
================


MongoDB = Get Stuff Done
------------------------

When choosing the stack for our TV guide service, we became interested in [NoSQL][nosql] dbs because we anticipated needing to scale horizontally. We evaluated several and settled on [MongoDB][mongodb]. The main reason was that MongoDB got out of the way and let us get work done. You can read a little more about our production setup [here](http://seanhess.posterous.com/surviving-a-production-launch-with-nodejs-and).  

So when you read that MongoDB is a [document store][doc], you might get the wonderful idea to store your relationships in a big document. Since mongo lets you [reach into objects](http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29), you can query against them, right?  

Several times, we've excitedly begun a schema this way, only to be forced to pull the nested documents out into their own collection. I'll show you why, and why it's not a big deal.  

Let's make a blog!
------------------

Here's an example. You just discovered that Mongo lets you store nested documents. You set out to make a blog.

{% highlight javascript %}
db.posts.save({ title: "My First Blog Post"
              , content: "Here is my super long post ..." 
              , created: 1328118062598
              })
{% endhighlight %}

What's a blog without comments? Hey, mongo is awesome! Let's nest the comments. Multiple collections are for RDBMS dinoasaurs. 

{% highlight javascript %}
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
{% endhighlight %}

This works nicely with the key/value idea that you should [denormalize][d] your data. You can avoid joins if all the data you need for a call is in one document. It's easy to treat these nested documents as if they were top-level. As long as your blog post displays the post and all comments, this works great:
 
{% highlight javascript %}
function postWithAllComments(id) {
    return db.posts.findOne({_id: id})
}

function addComment(postId, comment) {
    comment.created = Date.now()
    db.posts.update({_id: postId}, {$push: {'comments': comment}})
}
{% endhighlight %}

When Nested Documents Become a Problem
--------------------------------------

The minute you need just a little control over querying against those comments, you're stuck. For example, you might want to display all comments made by a certain user, or there are tons of comments on a post, and you want to page them. You're first tempted to try this:

{% highlight javascript %}
// bad idea
function commentsByUser(username) {
    return db.posts.find({"comments.name": username}, {comments: 1}).toArray()
}
{% endhighlight %}

Which seems to work, except it returns *all* the comments for each post they commented on. There's no way to get only the matched comment back. If there are 1000 comments per post, and the user commented on 3 posts, you'll have to wade through 3000 comments. 

Another thing that I need to do a lot is get a range of documents. Perhaps we want to see all comments made yesterday. You have the same problem: you get all the comments for each post. 

{% highlight javascript %}
// bad idea
function commentsBetweenTimes(start, end) {
    return db.posts.find( {"comments.created": {$gte: start, $lt: end}}
                        , {comments: 1}
                        ).toArray()
}
{% endhighlight %}

Collections are Cheap and [$in][in] is Your Friend
---------------------

It turns out it isn't hard to put the comments in their own collection. If you restructure your posts to look like this, everything falls into place:

{% highlight javascript %}
// db.posts
{ title: "My First Blog Post"
, content: "Here is my super long post ..." 
, comments: [ "4f297e550b3e6d9e2b7aa58e", "4f297e5c0b3e6d9e2b7aa58f" ] 
}

// db.comments
{ _id: "4f297e550b3e6d9e2b7aa58e" 
, text: "This post sucks!"
, name: "seanhess"
, created: 1328118162000 }
{% endhighlight %}

Your client doesn't have to be aware of the change, just use an [$in][in] query. It's a single join, but since both are indexed, it's plenty fast. The join is easy to write, and the comment-specific queries return only comments.

{% highlight javascript %}
function postWithAllComments(id) {
    var post = db.posts.findOne({_id: id})
    post.comments = db.comments.find({_id: {$in: post.comments}).toArray()
    return post
}

function addComment(postId, comment) {
    comment.created = Date.now()
    db.comments.save(comment)
    db.posts.update({_id: postId}, {$push: {'comments': comment._id}})
}

function commentsByUser(username) {
    return db.comments.find({"name": username}).toArray()
}

function commentsBetweenTimes(start, end) {
    return db.comments.find({"created": {$gte: start, $lt: end}).toArray()
}

{% endhighlight %}

While our postWithAllComments function requires a 2-step query (a join), it's very fast. Everything comes out of memory, from an index (make sure to index the fields you use). The other functions are fast or faster and are easy to write. 

Many-to-Many Relationships
--------------------------

It's impossible to model many-to-many relationships without storing them in a separate collection, unless you copy the data to every document. This is fine for tags, but can be unrealistic if your data set is large. It also means that like our blog you can't query the nested documents themselves. Here's a post with many-to-many tags.  

{% highlight javascript %}
db.posts.save({ title: "My Post", tags: ["awesome", "incredible"] })
db.posts.find({ tags: "awesome" }) // works!
{% endhighlight %}

Let's say you want to record users that are attending events. You could store this data a variety of ways, but it's very likely that you'll want to get both the users attending an event, and the events a user is attending. It doesn't make sense to put either users under event, or events under users. 

Even though you're forced to be relational, a HUGE advantage over SQL is that you don't have to have a separate joining collection. We can store the relationships like we did with the blog posts. Put the relationship on both objects if you want. 

{% highlight javascript %}
var party = { _id: "chessparty"
            , name: "Chess Party!"
            , attendees: ["seanhess", "bob"] }
var user = { _id: "seanhess", name: "Sean Hess", events: ["chessparty"]}
db.events.save(party)
db.users.save(user)

db.events.find({_id: {$in: user.events}}) // events for user
db.users.find({_id: {$in: party.attendees}}) // users for event
{% endhighlight %}


[Denormalization][d] might be important
-----------------------------------------------------

So, that said, sometimes you need your queries to be really fast. If your list of users only needs to show their names, you can nest their names along with their ids, allowing you to display your page without a join. You're not *storing* the users within events, your *copying* some of the fields you need to events, so you can skip the [$in][in] query. This is called [denormalization][d], and the [usual warnings about early optimization](/2011/12/15/optimization_is_like_firing_clay.html) apply.  

{% highlight javascript %} 
{ _id: "chessparty"
, name: "Chess Party!"
, attendees: [ {_id:"seanhess", name:"Sean Hess"}
             , {_id:"bob", name:"Bob Somebody"} ] 
}
{% endhighlight %}

Don't fight the Mongo
---------------------
MongoDB just lets you get stuff done. Don't become a [NoSQL][nosql] or [Document Store][doc] purist, just write code that works. It's the mongo way. It's easy to store relationships in a separate collection, and the joins are pretty cheap if you don't split up your data too much. Don't be overly tempted to store everything in a nested document, because at least in my experience, you end up needing to query against them sooner rather than later. 

[d]: http://en.wikipedia.org/wiki/Denormalization "Denormalization"
[in]: http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24in "MongoDB Advanced Queries - $in"
[doc]: http://en.wikipedia.org/wiki/Document-oriented_database "Document Oriented Database"
[nosql]: http://en.wikipedia.org/wiki/NoSQL "NoSQL"
[mongodb]: http://www.mongodb.org/ "MongoDB"

