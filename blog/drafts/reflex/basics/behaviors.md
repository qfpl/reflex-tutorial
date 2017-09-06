---
title: Behaviors
date: 2017-09-03
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

<div id="grid-setup"></div>

[Previously](../events/) we had a look at `Event`s, which describe values occurring at particular instants in time.
Now we'll have a look at the other half of the core FRP types - `Behavior`s.

## What is a `Behavior`?

A `Behavior` is something that has a value at _all_ points in time.

A `Behavior` in `reflex` looks like this:
```haskell
data Behavior t a
```

and we can think of it as being like a function from time to values:

```haskell
t -> a
```

We are working with a discrete-time FRP system, and so this function is going to be piecewise-linear, with any transitions happening when `Event`s occur.

Externally-triggered `Event`s give us our logical clock, and so we use `Event`s to build up our time-varying `Behavior`s.

The functions that we've seen so far are pure functions.
Given the same inputs they will respond with the same outputs, and they'll behave the same way in every frame.

We'll start to see various typeclass constraints appearing when we're doing something that is dependent on which frame we are in, or something that is going to introduce a change to the processing of future frames.

## Working with `Behavior`s

Let us start by creating a `Behavior`.

We can do this with `hold`, which is a method from the `MonadHold` typeclass:
```haskell
hold :: MonadHold t m
     => a
     -> Event t a
     -> m (Behavior t a)
```

This takes an initial value and an `Event` as input.
The `Behavior` starts has the initial value as its value until the first firing of the `Event`.
After that, the `Behavior` has the value the `Event` had at the last time it fired.

The use of the `MonadHold` typeclass constraint indicates that we're doing something that will have an effect on the behavior of the FRP network in future frames.
Under the hood, `hold` is modifying the FRP network in order to add some state.

We could use this to keep hold of the last colour that was clicked on:
```haskell
sampleBlue :: MonadHold t m 
           => Event t Colour 
           -> ?
sampleBlue eInput         = do
  -- bColour :: Behavior t Colour
  bColour <- hold Blue eInput
 
```

We now need a way to poke and prod at `bColour` so that we can see what is going on.

To get something we can display, we can use `tag` to sample from it using other `Event`s:
```haskell
tag :: Reflex t 
    => Behavior t a
    -> Event t b
    -> Event t a
```

Here we are taking a `Behavior` and an `Event` that we're using just to get hold of reference points of time.
We massage things so that the reference `Event` fires whenever we are interested in the value of the `Behavior`, and 
the output `Event` will fire at the same time but with the value of the `Behavior` at the times the `Event` is firing.

As an aside: notice that the `tag` function doesn't have a `MonadHold` typeclass constraint.
Since `Behavior`s have values at all points of time this will behave the same way - but with different values - in every frame where the input `Event` fires.
We don't need to modify the FRP network, or to read from a frame-specific value, or anything like that, which means `tag` is a pure function.

We can use `tag` to query the value of the `Behavior` we built up before:
```haskell
sampleBlue :: MonadHold t m 
           => Event t Colour 
           -> Event t () 
           -> m (Event t Colour)
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  pure $ tag bColour eSample
```

Let's have a play around with this:
<div id="basics-behaviors-sampleBlue1"></div>

If we start clicking "Sample", we'll see that it starts out `Blue` as we would expect.
If we then start clicking on combinations of "Red", "Blue", and "Sample", we'll see that it tracks whatever the user has clicked on.

The state doesn't change until the frame _after_ the firing of the `Event`s in `hold`.
We can see that by sampling from the `Behavior` when _any_ of the buttons are pressed:
```haskell
sampleBlue :: MonadHold t m 
           => Event t Colour 
           -> Event t () 
           -> m (Event t Colour)
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  pure $ tag bColour eAny
```

<div id="basics-behaviors-sampleBlue2"></div>

There are some parallels with the `State` monad here.
Inside of the monad, there is a value for the state at all points in time.
Any modifications to the state, or reads from the state, happen at discrete points of time.

We're a lot less explicit about time when working with the `State` monad, so it might take some squinting to see the parallels there.
It's also not as straightforward to compose computations working with `State s1 m` and `State s2 m` into a computation working with `State (s1, s2) m`, or to decompose a computation that works with `State (s1, s2) m` into computations that work with `State s1 m` and `State s2 m`.

TODO a bit on MonadSample / sample, why you possibly don't want it most of the time

<div id="basics-behaviors-sample"></div>

## Other functions on `Behavior`s

```haskell
attach          :: Reflex t 
                => Behavior t a 
                -> Event t b 
                -> Event t (a, b)
```

```haskell
attachWith      :: Reflex t 
                => (a -> b -> c) 
                -> Behavior t a
                -> Event t b
                -> Event t c
```

```haskell
attachWithMaybe :: Reflex t
                => (a -> b -> Maybe c) 
                -> Behavior t a
                -> Event t b
                -> Event t c
```

```haskell
gate :: Reflex t 
     => Behavior t Bool
     -> Event t a
     -> Event t a
```

## Interesting instances

```haskell
instance Reflex t => Functor (Behavior t) where ..
```

```haskell
sampleFlipBlue :: MonadHold t m
               => Event t Colour
               -> Event t ()
               -> m (Event t Colour)
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput
  let bFlippedColour = flipColour <$> bColour
  pure $ tag bFlippedColour eSample
```

<div id="basics-behaviors-sampleFlipBlue"></div>

```haskell
instance Reflex t => Applicative (Behavior t) where ..
```

```haskell
sampleAlwaysBlue :: Reflex t
                 => Event t Colour
                 -> Event t ()
                 -> Event t Colour
sampleAlwaysBlue eInput eSample =
  tag (pure Blue) eSample
```

<div id="basics-behaviors-sampleAlwaysBlue"></div>

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2
  let bPair = (,) <$> bColour1 <*> bColour2
  pure $ tag bPair eSample
```

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2
  pure $ (,) <$> bColour1 <*> bColour2 <@ eSample
```

<div id="basics-behaviors-samplePair"></div>

As usual, `reflex` wants to be your friend and so provides all kinds of other instances that you might find a use for:

```haskell
instance  Reflex t              => Monad    (Behavior t  ) where ...
instance (Reflex t, Monoid a)   => Monoid   (Behavior t a) where ...
instance (Reflex t, Num a)      => Num      (Behavior t a) where ...
instance (Reflex t, IsString a) => IsString (Behavior t a) where ...
```

## Next up

`Event`s and `Behavior`s are the core types of FRP.

The `reflex` library also has a type that combines the two together.
This is done for performance reasons, but also nicely encapsulates a pattern from FRP folklore.

In the [next post](../dynamics/) we'll look at the combination of the two - `Dynamic`s.
