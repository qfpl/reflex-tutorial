---
title: An introduction to `reflex`
date: 2017-09-01
authors: dlaing
project: reflex
---

## What is Functional Reactive Programming (FRP)?

There has been a lot of interest in FRP over the last few years, and there are now a lot of different meanings in use.

My claim is that the FRP implementations that are closer to the original idea are much more useful and powerful than some of the things that have come afterwards.
We'll have to go a little way down the rabbit hole with the ideas and concepts before I can make a solid case for that.

The original definition of FRP required two things:

- clear denotational semantics
- support for continuous time

The implementations that I prefer usually:

- have clear denotational semantics
- use `Event` and `Behavior` types
- have the ability to do higher-order FRP, so that the network of `Event`s and `Behavior`s can be changed over time.
- have come up with APIs to greatly reduce the risk of introducing space or time leaks

These systems give you first-class values that model mutable state, with a built-in and souped-up version of observers, and the clear denotational semantics mean that abstractions provided by these systems all compose well.

The `Event` values model changes that happen at single points of time, like a user clicking a button or a timeout expiring. 
The `Behavior` values model state that has values at all points of time, like the position of a mouse or whether a user has admin permissions on a site.
The `Behavior`s are created, changed, and queried by `Event`s - and they have an `Applicative` instance so that we can combine them.

This leads to a very different way of dealing with state than most people are used to.
It's very powerful, as we'll soon see.

So far, the implementations that I have used have not had support for continuous time.
Instead, they work with a discrete moments time.
This means that there are some thoughts that we can't really think in this system, but up to this point that hasn't been a problem for me.
If someone has a library that supports continuous time as well as the above points, I'd be very keen to take a look.

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
The key trick to doing that is to have efficient procedures to find differences between DOMs and to patch them, so that you can translate changes in the virtual DOM into changes in the actual DOM with the least amount of work.

The `react` library - as well as many others - use this, and lots of people are quite enthused by it.
It seems a bit like a functional approach to the problem, which is great.
If you're struggling to deal with a mutable tree with mutable state at the branches and at the leaves, you'll reach for whatever seems like it'll work.

Some folks have tried to use an even more functional approach, and work with the DOM like they are working with a pure function:

```
DOM -> DOM
```

The problem with that is the assumption that the DOM is stateless.
That assumption is incorrect.
We only need to look at a text input to see pieces of state - the text in the input, the position of the cursor - that isn't captured in the DOM.

This means that to work with those kind of inputs you have two choices.

You can have local state, like `react` does.
This mean you have a mutable tree with mutable state at the branches and at the leaves, but with different types and granularity to what you had with the DOM.

The alternative is to pull out the state you are interested in from all through the tree, and to put it back in to the same places when you are rendering the DOM.
This gives you a mutable tree with mutable state at the branches and at the leaves which you layer over your DOM in your event loop.

This might be a big global object, or you might be able to scope it a little more tightly to where you are using the state.
You can see approaches like these with `redux` and with `halogen`.

My claim is that once you're comfortable with `reflex`:

- you'll be able to work with mutable trees with mutable state at the branches and at the leaves, in a functional way, with tools that allow your code and your reasoning about the code to compose well
- you'll be able to make the same alterations to the DOM that the virtual DOM approach does, but without having to do the diffing or patching steps
- you might be a little perplexed by some of the other approaches in this space

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
