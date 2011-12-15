---
layout: post
title: Bad Optimization is like firing clay
---

{{ page.title }}
================


When "optimized" doesn't mean better
------------------------------------

*To Optimize* literally means to find the best possible solution. It's hard to see how that could be bad, since it means to make something better. However, another definition is to make something better for a specific purpose, at the expense of others. In that case, sometimes it's not the right choice.

> Optimization is only bad when you trade flexibility for performance. 

Flexibility here means simple, maintainable code, or just time. Get rid of either and it'll be difficult to change your mind. Go ahead and do obvious things in the name of performance. As long as you don't spend too much time or make things complicated, you can't go wrong. 

But pay attention when you come to a trade-off — where you must make something more complicated or spend extra time. You probably want to wait.

I have to learn this again?
---------------------------

For our new product (a [backbone][b] app), our CEO suggested that we support up to 10,000 items on a page at once. We were able to make it perform well *by assuming that each item had a static height*. This allowed us to calculate the exact position of each item on the screen without drawing it. We coudl then load and unload items as the user scrolled. After a few days of work, we acheived great performance, even for 10,000 items. 

Then, our designer showed us higher-fidelity designs. The items needed dynamic heights. 

We went back to work, and I was surprised by how long it took to get the items to display correctly. It took nearly as much time to support dynamic heights as it would to write simple layout code from scratch.

In exchange for large scale performance, we traded away our initial time, as well as the ability to work with dynamic heights. Instead, we could have made it perform well for 100 items from the beginning, and we would have known about the dynamic heights before optimizing it for more. 

Clay is either flexible, or strong
----------------------------------

[Clay will stay flexible][c] indefinitely[^1]. You can work something into a shape, and come back later and change the shape. At some point you can [fire the clay][firing], making it rigid and exchanging its flexibility for increased strength. 

![Clay Model](http://src.sencha.io/600/http://upload.wikimedia.org/wikipedia/commons/6/6c/Renault_clay_model_-_front.JPG)

If you are making a model out of clay, there's a clear advantage to firing it. It makes the model stronger. A stronger model is "better" — it can be handled without fear of deforming it. 

Most of the time, though, a model is strong enough without firing. You might need it to be strong *eventually*, but for right now it serves its purpose well — to model something. If you keep it unfired, you can add more detail, change its shape, even change its purpose without starting over. 

These are all improvements you can make without sacrificing anything. It's just better. But to make it *stronger*, you have to trade away its flexibility. 

Don't make tradeoffs until you have to
--------------------------------------

Software needs to perform, but only well enough. No one will notice if you shave 50ms off of a 300ms page load. You usually have a fairly good idea of what constitutes acceptable performance.

On the other hand, you usually have *no* idea what changes you might have to make. Design requirements change all the time. Sometimes, you don't even know the best way to make something until you've tried building it once. 

In every case it's easier to make major investments of time and complication in the name of performance after your code has stopped changing, simply because you don't have to undo anything. 

But what if you delay for so long that you forget to go back and optimize? This is actually the *best* thing that could happen. Most of the time we don't need the performance we think we do. If we do, it'll come up again. If we don't, we can let our simple, stable code perform its function without bothering it. 

As it turns out, we didn't need our optimization at all. There's no way the current UX can support 10,000 items, so making our code complicated to achieve that level of scale was a mistake. Thinking clearly about the difference between improvements (adding details to the clay) and trade-offs (firing the clay) will help prevent similar mistakes in the future. 















<!--


















Maybe you just need to see your first version to realize what you really want to do. 












Maybe you absolutely do need it to be strong for some reason. You might as well wait until the current strength isn't enough. 










> A product designer uses clay to make 3D model of a prototype car. He works hard and the finished model looks great. He shows it off to his boss, and barely manages to stop his boss before he tries to pick it up. It's still soft, because he hasn't fired it yet. 

It would be more convenient to fire it right? He will want to fire it eventually, (or put it into a CAD program). It would make a *better model* if it were fired. 

> Our designer decides to fire it that night, so he can show it off without worrying about anyone touching it. He and his boss take it to the lead engineer, who points out that (learns something because he saw the model.... understands more)









-- This is a poort example, because it wouldn't make a very good plate without being fired. It's not good at all. It'd be better if you think of it as a model, not a machine part. So, something like a model. 

> You are an early hunter/gatherer. You have some food, but eating in the dirt is for those savages across the river. You need a plate! You head down to the river to get some clay while your companion prepares the food. You pound it flat, getting it nice and wide so you can eat lots of food at once. Finally, you go throw it in your kiln (you've done this before), and out comes a nice, sturdy plate. 
>
> You head back to your companion to show it off, and find her making stew. You look at your plate in dismay, then head back to the river to make a bowl.














In theory, I understand that early optimization is bad. I've [read so][p] [several times][w]. I've "learned" it the hard way. 





Yet somehow I find myself once again needing to make major changes to optimized code. I find the hardest part can be distinguishing between optimization and "planning ahead", or architecture. 
















In theory, I understand that early optimization is bad. I've [read about it][p] [several times][w]. I've "learned" it the hard way. Yet somehow I find myself once again needing to make major changes to optimized code. I find the hardest part can be distinguishing between optimization and "planning ahead", or architecture. 

> Premature optimization is the root of all evil — Donald Knuth

This time around, it happened because for our new product (a [backbone][b] app), our CEO suggested that it needed to work smoothly for up to 10,000 items on a page at once. We created a system that would load and unload those items onto the page as the user scrolled. We were able to make it perform well by assuming that each item had a static height. We could calculate the exact position of each item on the screen without drawing it. After a few days of work, we were able to get great performance, even for 10,000 items. 

Then, our designer showed us higher-fidelity designs for the items. They need dynamic heights. 

We went back to work, and were surprised by how long it took to get the items to display correctly, even without optimization. It took nearly as much time to support dynamic heights as it would to rewrite the layout code from scratch. 

After going through this, I thought of an analogy that helps me think clearly about optimization.

Clay is flexible, or strong
---------------------------

[Clay will stay flexible][c] indefinitely[^1]. You can work something into a shape, and come back later and change the shape. At some point you can "fire" the clay, and trade its flexibility for strength. 

Let's say you are creating something out of multiple clay pieces. You make a piece with 2 knobs sticking out, and it works fine. You have a decision to make: should you fire the piece? It works great, and you know it needs to be strong. Why not?

Well, a day after you fire the piece, you realize that you need 3 knobs, not 2. Now, you have to make a new piece. If you had waited, everything would have been fine, because it didn't need be strong **yet**. It is easier to fire the pieces after you are more sure of their shape.  

Trade flexibility for performance?
--------------------------------

Sometimes there is a direct trade-off between performance and flexibility. You can choose to bake your code, making it performant, but it's harder to work with later. 












Optimize doesn't mean to make something awesome. It means to make something better at the expense of other things. 










So, when you have to decide between strength (performance) and flexibility, choose flexibility until you must choose performance.

This is the difference between optimization and architecture — optimization is a direct trade-off between performance and flexibility. 

Planning Ahead
--------------

Making your code inflexible is very different from planning ahead. 


##### Notes 





#### Stuff

Many painters I know never thing of a painting as "finished". Instead, it's simply abandoned at some level of progress. They always want to come back and add a few touches. Sculptors have to be much more decisive. They must decide when to fire the clay. 






Optimization is like firing a piece of clay. It makes it so you can't change it any more. 

We've all heard that pre-optimization is bad, but it's sometimes hard to articulate why. It's also easy to pre-optimize in the name of "planning ahead". There's a difference between the two, though, and I thought of an analogy that helps me understand. 

Oil based clays will stay supple indefinitely. You can work something into a shape, and come back later and change the shape. Let's say you have to create a complicated system out of clay parts. You need one part that looks like a cube with a rod sticking out of it. You make your piece, and it comes out fine. You have a decision to make: should you fire the clay?

To fire clay is basically to bake it. It makes it hard. After it is fired, the clay is strong, but no longer flexible. If you need to change it, the only thing you can do is stick a new piece of clay onto it. 

Should you fire the piece you just made?

The answer depends on how well it works unfired. If your final system doesn't need to support a lot of wait **yet**, even if it does eventually, it's better if you don't fire it. 

Now let's say that the master clay-person comes along and says you need to add a second knob to your piece. If you already fired the clay, you either have to create a very weakly attached new knob, or re-create the entire piece. If you haven't fired it yet, you can just add it and fire it later. 

When should you fire the clay? The minute you need to support that weight. 

Good architecture, or "Planning Ahead", is flexible and simple. Changes are easy, because the author worked hard to make a piece compatible with several different options. Optimization, on the other hand, is rigid and complex. It's un-changeable. You should wait to make your piece rigid until you need to. Hopefully never. 


Story
    Story
    We traded both time and flexibility for scrolling speed.
    What would have happened if we made the trade-off later?
With Clay, you can trade flexibilty for strength (performance)
    Clay
    Firing does make it a better bowl/plate
    But an unfired bowl is better than a fired plate
Don’t make tradeoffs until you have to Expect the unexpected : Changing design requirements Better understanding of the problem Harden something after it STOPS moving (What if we forget?) Best Case: Maintenance - maybe you can leave it unfired - in its simplest form - Maybe you don’t actually need the performance
-->

[^1]: This is more true of polymer clay than ceramic, which must be kept wet to stay flexible. 

[p]: http://c2.com/cgi/wiki?PrematureOptimization "Premature Optimization"
[firing]: http://en.wikipedia.org/wiki/Pottery#Firing "Pottery"
[w]: http://en.wikipedia.org/wiki/Program_optimization "Program Optimization"
[b]: http://documentcloud.github.com/backbone/ "Backbone.js"
[c]: http://en.wikipedia.org/wiki/Modelling_clay "Modeling Clay"
