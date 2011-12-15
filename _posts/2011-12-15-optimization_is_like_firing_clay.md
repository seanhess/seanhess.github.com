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

Flexibility here means simple, maintainable code, or just time. Get rid of either and it'll be difficult to change your mind. Plan ahead, and do obvious things in the name of performance. As long as you don't spend too much time or make things complicated, you can't go wrong. 

But pay attention when you come to a [trade-off][t] — where you must make something more complicated or spend extra time. You probably want to wait.

I have to learn this again?
---------------------------

For [our](http://corp.i.tv) new product (a [backbone][b] app), our CEO suggested that we support up to 10,000 items on a page at once. We were able to make it perform well *by assuming that each item had a static height*. This allowed us to calculate the exact position of each item on the screen without drawing it. We coudl then load and unload items as the user scrolled. After a few days of work, we acheived great performance, even for 10,000 items. 

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

But what if you delay for so long that you forget to go back and optimize? This is actually the *best* thing that could happen. Frequently we don't need as much performance as we think. If we do, it'll come up on its own. If we don't, our forgotten, simple, and stable code can perform its function in peace.

> Optimize — Make improvements freely, but delay trading time or simplicity

As it turns out, we didn't need our optimization. There's no way the current UX can support 10,000 items, so making our code complicated to achieve that level of scale was a mistake. Thinking clearly about the difference between improvements (changing the figure) and trade-offs (firing the clay) will help us wait until [the right time][when].

[^1]: This is more true of polymer clay than ceramic, which must be kept wet to stay flexible. 

[firing]: http://en.wikipedia.org/wiki/Pottery#Firing "Pottery"
[b]: http://documentcloud.github.com/backbone/ "Backbone.js"
[c]: http://en.wikipedia.org/wiki/Modelling_clay "Modeling Clay"

[x]: http://c2.com/cgi/wiki?PrematureOptimization "Premature Optimization"
[when]: http://en.wikipedia.org/wiki/Program_optimization#When_to_optimize "Program Optimization - When to optimize"
[t]: http://en.wikipedia.org/wiki/Program_optimization#Trade-offs "Program Optimization - Trade-offs" 
