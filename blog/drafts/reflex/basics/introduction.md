---
title: An introduction to reflex
date: 2017-09-01
authors: dlaing
project: reflex
---

This is a series of posts aimed at getting people started with Functional Reactive Programming using the `reflex` library.

It assumes that you're comfortable with the Haskell language, up to and including the `Applicative` and `Monad` typeclasses.
If you're not there yet, I usually recommend the [CIS194 course](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).
Some of us relayed these lectures to our local meetup group, so we have you covered if you have a preference for [videos of lectures](https://github.com/bfpg/cis194-yorgey-lectures).
I've also heard glowing praise for [The Haskell Book](http://haskellbook.com/).

There is also a book on [Functional Reactive Programming](https://www.manning.com/books/functional-reactive-programming) by Stephen Blackheath and Anthony Jones, which I also highly recommend.
It does a great job of motivating and explaining FRP.
It uses the `sodium` library, which has bindings for a lot of languages, and most of the examples in the book are in Java.

## What is Functional Reactive Programming (FRP)?

There has been a lot of interest in FRP over the last few years, and there are now a lot of different meanings of FRP that are in use.

My claim is that the FRP implementations that are closer to the original idea are much more useful and powerful than the others.
We'll have to go a little way down the rabbit hole with the ideas and concepts before I can make a solid case for that.

The original definition of FRP had:

- `Event` and `Behavior` types

and required:

- clear denotational semantics
- support for continuous time

The `Event` and `Behavior` types are at the heart of these systems.
The `Event` values model changes that happen at single points of time, like a user clicking a button or a timeout expiring. 
The `Behavior` values model state that have values at all points of time, like the position of a mouse or whether a user has admin permissions on a site.
The `Behavior`s are created, changed, and queried by `Event`s, and they have an `Applicative` instance so that we can combine them.

This leads to a very different way of dealing with state than most people are used to.
It's very powerful, as we'll soon see.

The implementations that I prefer usually:

- have `Event` and `Behavior` types
- have clear denotational semantics
- have the ability to do higher-order FRP, so that the network of `Event`s and `Behavior`s can be changed over time.
- have come up with APIs to greatly reduce the risk of introducing space or time leaks

These systems give you first-class values that model mutable state, with a built-in and souped-up version of observers.
The clear denotational semantics mean that abstractions provided by these systems all compose well.

The implementations that I have used provide discrete-time FRP rather than continuous-time FRP.
The difference is similar to the difference between a digital and analog circuit.
Discrete-time FRP uses `Event`s as the clock that drive the circuit, where continuous-time FRP does not.
This means that with continuous-time FRP we can manipulate sampling and update rates explicitly, which is very handy when you are working with multimedia.
It also leads to much simpler denotational semantics than the discrete-time FRP systems have

While this means there are some thoughts we can't express in discrete-time FRP system, I've been finding them very useful nonetheless.
If someone has a library that supports continuous-time FRP as well as the above points, I'd be very keen to take a look so I can discover what I've been missing out on.

The other interesting thing about the implementations that I tend to like is that they have a kind of phase separation.
The API that is presented to the user is typically building up a data structure that describes a FRP network and how it will change over time.
Given that, the library will effectively compile this into an efficient graph of switches and latches, along with all of the memory management required to have it play nicely with the garbage collector of their chosen language.
This means there are opportunities to increase the performance or decrease the memory usage of the system while having relative stable user facing APIs.
I'm excited about where that might lead.

Documentation is probably the biggest gap with these systems at the moment, although I have a lot of things I'd like to write which I hope will help on that front.

## What is `reflex`?

The `reflex` library is an implementation of an FRP library that satisfies all of the preferences that I listed above.
It is also highly performant and has had some great work done around its usability.

You can use `reflex` to write code via either GHC and GHCJS, although there has been a focus on using it to create web front-ends via GHCJS the `reflex-dom` companion library.

There is support for writing new back-ends as well, and so you can use it in other places where having a better model of mutable state might be handy, such as:

- command line applications
- servers
- SDL applications

We'll take a look at that support later on, but for now we'll be focusing on using `reflex` to create web front-ends.

### Aside: What is the virtual DOM?

Inside the browser, the Document Object Model (DOM) is a mutable tree with mutable state at the branches and at the leaves.
That doesn't sound ideal to a lot of folks.
It can also be slow to search and to modify.

To work around that, some clever people worked out that they could build a data structure to model the DOM - the virtual or shadow DOM - and operate on that instead.
The trick to doing that is to have efficient procedures to find differences between DOMs and to patch them, so that you can translate changes in the virtual DOM into changes in the actual DOM while minimizing the amount of work done.

The `react` library and many other libraries use this idea, and lots of people are quite enthused by it.
It seems a bit like a functional approach to the problem, which is great.
If you're struggling to deal with a mutable tree with mutable state at the branches and at the leaves, you'll reach for whatever seems like it'll work.

Some folks have tried to use an even more functional approach, and work with the DOM like they are working with a pure function:

```
DOM -> DOM
```

The problem there is that it is built on the assumption that the DOM is stateless.
That assumption is incorrect.
We can look at a text input to see pieces of state that aren't captured in the DOM, like the text in the input and the position of the cursor.

There are two ways libraries tend to work with those kind of inputs.

You can have local state, like `react` does.
This mean you have a mutable tree with mutable state at the branches and at the leaves, but the tree has a different type and granularity to what you had with the DOM.

The alternative is to extract all of the state that you are interested in from the DOM tree, manage and change it, and then use it to restore the state during the next update to the DOM tree.
This gives you another mutable tree with mutable state at the branches and at the leaves which you layer over your DOM in your event loop.

This might be a big global object, or you might be able to scope it a little more tightly to where you are using the state.
You can see approaches like these with `redux` and with `halogen`.

My claim is that once you're comfortable with `reflex`:

- you'll be able to work with mutable trees with mutable state at the branches and at the leaves, in a functional way, with tools that allow your code and your reasoning about the code to compose well
- you'll be able to make the same alterations to the DOM that the virtual DOM approach does, but without having to do the diffing or patching steps

In the Haskell community we periodically hear from people who work with non-functional programming languages and have dived into Haskell, and start to like their work language much less afterwards.
There's a bit of a risk of something happening if you're working with some of the other popular libraries and frameworks, but if you have enough Haskell under your belt for this series you have probably accepted and dealt with that risk already.

## Getting started

The easiest way to install the `reflex` library is via the `reflex-platform`:
```
> git clone https://github.com/reflex-frp/reflex-platform
```

This uses Nix behind the scenes to set things up, so we don't have to jump through any hoops to set up GHCJS and things like that.

For the most part we don't need to know any Nix to make use of this, because `reflex-platform` comes with some handy shell scripts.

If we have a `cabal` based project lying around that we want to compile with GHCJS, we can use:
```
> cd my-cabal-project
> PATH_TO_REFLEX_PLATFORM/work-on ghcjs ./.
> cabal configure --ghcjs
```
and if we want to compile it with GHC:
```
> cd my-cabal-project
> PATH_TO_REFLEX_PLATFORM/work-on ghc ./.
> cabal configure
```

We'll look at using Nix to do much fancier things later on in this series.

## Next up

As I've mentioned, there are two main types at the heart of the more powerful FRP libraries: `Event`s and `Behavior`s.

In the [next post](../events/) will start by looking at `Event`s.
