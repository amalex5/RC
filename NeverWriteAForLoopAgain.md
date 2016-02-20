NEVER WRITE A FOR LOOP AGAIN
==============

or, fun folding with map/reduce/filter
-----

#### andrew alexander 

(This is a loose transcription (in some parts, an expansion) of a five-minute talk I gave at the Recurse Center on February 18th, 2016. All of the code I live-coded, so the written version loses some of the pedagological power of narrating my thought process as I type. The code below happens to be JavaScript, but most languages have equivalents.)

So often it happens that we have a some stuff stored in a list, and we want to do something to every element of that list. Suppose we have this list:

    myList = [1,2,3,4,5]

What if we want to multiply all these values by 10? We can write a `for` loop!

    for (var i = 0; i < myList.length; i++){
    	myNewList.push(myList[i] * 10)
    }
    >> Uncaught ReferenceError: myNewList is not defined

Whoops, we didn't define `myNewList`! let's go back and do that:

    myNewList = []
    for (var i = 0; i < myList.length; i++){ \
    	myNewList.push(myList[i] * 10)
    }
    myNewList
    >> [10, 20, 30, 40, 50]

There we go! But that's a lot of writing. We have to do all that pedantic boilerplate: we initialize the counter `i`, we tell the loop to add `1` to it every time, we make sure that it's less than the length of our list; then, inside the loop we have to call `myList[i]` just to get at the thing want... what a pain! But it turns out that THERE'S A BETTER WAY!!!!!!

    myList.map( function(c) {return c*10} )
    >> [10, 20, 30, 40, 50]

WOOO! LOOK AT HOW MUCH LESS WRITING THAT IS. This is the awesome function `map`, which takes as its input a function, and, um... well, anyway, [don't worry about how or why it works](https://en.wikipedia.org/wiki/Language-game_(philosophy)) for now. Just look at how pretty and clean it is!

Note that it doesn't modify `myList`:

    myList
    >> [1,2,3,4,5]

So if we want to save it somewhere, we can just set a variable to it:

    myMultiplesOfTen = myList.map( function(c) {return c*10} )

Okay. So `map` is awesome! But here's another question. Sometimes we have a list, and we want to transform it into a subset of itself: like, for example, maybe we have a list of numbers, and we want to get only the elements from that list that are even numbers. Well, we could do a `for` loop:

    evens = []
    for (var i=0; i<myList.length; i++){
    	if (myList[i] % 2 == 0) {
    		evens.push(myList[i])
    		}
    	}
    evens
    >> [2,4]

(Note that here we're testing whether they're even by using this modulo operator `%`; numbers are even if their remainder when divided by two is zero.) Again: this is so much writing! THERE'S A BETTER WAY!

    myList.filter( function(e){return e % 2 == 0} )
    >>[2, 4]

So `filter` is kind of like `map`. We put inside `filter` a function that

1. takes each element of the list in turn
2. does some stuff (maybe a lot of stuff, maybe a little)
3. returns either `true` or `false`

And if it returns `true`, the element gets added to our output list; if it returns `false`, the element gets forgotten. (The fancy name for this kind of function is a "predicate": a function that takes some stuff and returns either `true` or `false`.)

Third hypothetical situation! `map` and `filter` both let us analyze each element of our list independently. But sometimes we want to do things on lists, and we don't want to treat each element *independently*, but want to somehow *relate* them to each other. For example, what if we want to add up all the values in `myList`? Well, we could do another `for` loop:

    for (var i = 0; i < myList.length; i++){ 
    	mySum += myList[i]
    	}
    >> Uncaught ReferenceError: mySum is not defined

Oh, crap; we once again forgot to define the variable in which to store our sum. What a pain! Let's try that again:

    mySum = 0
    for (var i = 0; i < myList.length; i++){ 
    	mySum += myList[i]
    	}
    mySum
    >> 15

OK, fine. But this so much writing! All I want to do is add up some numbers. Once again: THERE'S A BETTER WAY! `map` has a cousin we can call:

    myList.reduce( function(prevVal,currVal){ return prevVal + currVal }
    		, 0  )
    >> 15

So `reduce` takes a list and turns it into a single element, by doing some operation successively on each element in the list. In this case, each time it grabs a new element from the list (`currVal`), it adds it to the growing sum (`prevVal`). There's no "previous value" the first time we call it, of course, so we start things off by setting it equal to `0` (in the second argument to `reduce`).


So that's `map`, `reduce`, and `filter`! More generally, these are all examples of [folding functions](https://en.wikipedia.org/wiki/Fold_(higher-order_function)), which are really cool. 