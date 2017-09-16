---
title: Dynamics
date: 2017-09-21
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

<div id="grid-setup"></div>

[Previously](../behaviors/) we had a look at `Behavior`s, which finished our coverage of the two main FRP types.

Now we'll have a look at an additional tool that `reflex` gives us - `Dynamic`s.

## What is a `Dynamic`?

A `Dynamic` is the combination of an `Event` and a `Behavior`.

A `Dynamic` in `reflex` looks like this:
```haskell
data Dynamic t a
```
and we can think of it as being a pair of an `Event` and a `Behavior`:
```haskell
(Event t a, Behavior t a) 
```

The `Behavior` carries some state around, and the `Event` fires with the value of the new state whenever the state is changed.
This is useful for reasons of efficiency.
`Behavior`s are pull-based, which means we need to poll them for changes, so combining a `Behavior` with an `Event` that fires when it updates means that we can write code that reacts to changes in state at the time when the changes occur.

This is very useful when we want to update a piece of the DOM.
A `Dynamic t Text` can be passed from where it was created, through the application, down to a DOM node that needs to display some changing text.
The `Behavior t Text` takes care of tracking the current state of the text to display, and `reflex-dom` can set up some code to replace the text in the DOM node whenever the `Event` signals that there is a change.

The `reflex` and `reflex-dom` libraries aren't prescriptive about how you structure your application, but the common advice is to pass `Dynamic`s as far down as possible into the code that generates DOM.
If you follow that advice then you can arrange things so that your code is doing the same changes to the DOM as the various virtual DOM libraries would, but skipping the need to diff and patch the various DOM trees.

There is some reference to building an `Event` and `Behavior` simultaneously in the `reactive-banana` documentation, and it appears in one of the examples, and so I suspect that the idea behind the `Dynamic` is probably lurking in the background or in the folklore of other `Event`-and-`Behavior` FRP systems.

## Working with `Dynamic`s

We can get hold of the `Event` if we need it:
```haskell
updated :: Reflex t 
        => Dynamic t a 
        -> Event t a
```
and we can get hold of the `Behavior`:
```haskell
current :: Reflex t 
        => Dynamic t a 
        -> Behavior t a
```

We also still have the handy typeclass instances that were available for `Behavior`s:
```haskell
instance Reflex t => Functor     (Dynamic t) where ...
instance Reflex t => Applicative (Dynamic t) where ...
instance Reflex t => Monad       (Dynamic t) where ...
```

We have to construct them directly, rather than combining existing `Event`s and `Behavior`s, so that the two components stay synchronized.

There is an equivalent of `hold` but for `Dynamic`s:
```haskell
holdDyn     :: (Reflex t, MonadHold t m) 
            => a 
            -> Event t a 
            -> m (Dynamic t a)
```
and there is a variant that lets us fold a function through a series of `Event` firings:
```haskell
foldDyn     :: (Reflex t, MonadHold t m, MonadFix m) 
            => (a -> b -> b) 
            -> b 
            -> Event t a 
            -> m (Dynamic t b)
```
which we'll often be using with the function application operator:
```haskell
($) :: (c -> d) -> c -> d
```

In order to unify:
```haskell
($) :: (c -> d) -> c -> d
```
and:
```haskell
       a        -> b  -> b
```
we need `c ~ d`.

We then have:
```haskell
($) :: (c -> c) -> c -> c
```
being unified with:
```haskell
       a        -> b  -> b
```
and so `a ~ (c -> c)` and `b ~ c`.

After the dust settles, we have:
```haskell
foldDyn ($) :: (Reflex t, MonadHold t m, MonadFix m) 
            => c 
            -> Event t (c -> c)
            -> m (Dynamic t c)
```

The `MonadFix` constraint is being used because `foldDyn` uses value recursion internally.

## An example of using `Dynamic`s

We're going to build a simple counter. 

To do that we're going to build a up a `Dynamic t Int`, which will start at `0` and will be transformed when various `Event`s fire.

We can use `foldDyn` to get started:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => m (Dynamic t Int)
counter =
  foldDyn ($) 0 $ 
    _
```

We'll add an `Event` corresponding to the pressing of an "Add" button:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> m (Dynamic t Int)
counter eAdd =
  foldDyn ($) 0 $ 
    _
```

When that button fires, we'll need to use that `Event` to supply a function of type `Int -> Int`:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> m (Dynamic t Int)
counter eAdd =
  foldDyn ($) 0 $ 
    _       <$ eAdd
```
and fortunately we have just the thing:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> m (Dynamic t Int)
counter eAdd =
  foldDyn ($) 0 $ 
    (+ 1)   <$ eAdd
```

<div id="basics-dynamic-counter-1"></div>

It is really common in FRP systems like these to deal with `Event`s which have functions for values, which is worth remembering when you're starting out and trying to solve problems like these for the first time.
It can seem really strange at first but you will get comfortable with it if you practice for a while and try to remember that functions are values too.

Let's modify our counter so that we can reset it.

We'll start with what we had before:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> m (Dynamic t Int)
counter eAdd =
  foldDyn ($) 0 $
      (+ 1)   <$ eAdd
```
and add an `Event` that will fire when "Clear" is pressed:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter eAdd eClear =
  foldDyn ($) 0 $
      (+ 1)   <$ eAdd
```

We are planning on creating some buttons to produce these `Event`s, and so the `Event`s won't happen simultaneously.
However, since we have separated out the logic from the controls, and are taking `Event`s as inputs, we can't guarantee that the `Event`s will be happening in different frames.

We are working with `Event t (Int -> Int)`, so we'll combine the `Event`s using `mergeWith` and function composition:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , _
    ]
```

Now we just need to put the `Event` in place:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , _          eClear
    ]
```
and supply a suitable function:
```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
```

<div id="basics-dynamic-counter-2"></div>

So far, so good.

## Bringing `RecursiveDo` into the picture

`Behavior`s have values at all points in time, and `Event`s only have values at certain instants in time.
This means that all `Behavior`s have a value before any `Event`s in the application fire, and so any `Event` can be used to sample from a `Behavior`.

We can use one firing of a particular `Event` to build up a `Behavior` and a later firing of the same `Event` to sample a `Behavior`.
Going further than this, we can use one firing of a particular `Event` to both build up and sample from a `Behavior`.
The reason this is fine is that `hold` updates the `Behavior` in the next frame rather than the current frame.

The result of this is that we are able to have loops in the graph of our FRP network.
This is fine, and is often very useful, but we need a language extension to be able to input them into Haskell in a convenient manner.

Imagine that we had an application that contained a counter that we wrote earlier, and that we wanted to specify an upper limit for the value of the counter.

We could add something like this to the settings page for the application:
```haskell
limit :: (Reflex t, MonadFix m, MonadHold t m) 
      => Event t () 
      -> Event t ()
      -> Event t ()
      -> m (Dynamic t Int)
limit eStart eAdd eClear = do
  eLoadLimit <- performEvent (loadLimitDb <$ eStart)

  dLimit <- foldDyn ($) 5 . mergeWith (.) $ [
      const   <$> eLoadLimit
    , (+ 1)   <$  eAdd
    , const 0 <$  eClear
    ]
    
  performEvent_ (saveLimitDb <$> update dLimit) 
    
  return dLimit
```

<div id="basics-recursiveDo-1"></div>

and then plumb the results into a revised form of our counter.

We would start with our old counter:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter eAdd eClear = do
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]

  pure dCount
```
and then pass in the limit:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = do
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]

  pure dCount
```

We can then check if we are within the limit:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = do
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  let dLimitOK = (<) <$> dCount <*> dLimit
    
  pure dCount
```
and use that to create a version of `eAdd` which only fires if we are within the bounds of the limit:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = do
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd
    
  pure dCount
```

Now all we have to do is replace the use of `eAdd` in the `foldDyn` with `eAddOK`.

That is going to look a little weird and fail to compile, due to the cyclic dependency it introduces:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = do
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  pure dCount
```
but we can resolve this by add the `RecursiveDo` language pragma and replacing the the `do` keyword with `mdo`:
```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  pure dCount
```

If you play around with this:
<div id="basics-recursiveDo-3"></div>
you'll see that it works, and that it is linked to the `limit` widget above.

If we hadn't needed the `MonadFix` constraint in order to use `foldDyn`, we would need it now in order to use `mdo`.

We could take this further, and create a data type to manage the settings:
```haskell
data Settings =
  Settings {
    settingLimit :: Int
  , settingStep  :: Int
  }
```

We can pull the settings apart for use in our counter:
```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Settings
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dSettings eAdd eClear = mdo
  let 
    dLimit = settingsLimit <$> dSettings
    dStep  = settingsStep  <$> dSettings

  let check c s l = c + s <= l
      dLimitOK    = check <$> dCount <*> dStep <*> dLimit
      eAddOK      = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+  )   <$> tag (current dStep) eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```
and we can build the settings up from the individual pieces on our hypothetical settings page:
```haskell
counter (Settings <$> dLimit <*> dStep) eAdd eClear
```

<div id="basics-recursiveDo-4"></div>

We're still playing with basic examples, but hopefully these examples plus a bit of imagination are enough to help see the usefulness of building up, passing around and pulling apart first-class values for state management.

## Removing extraneous updates

There is a small trap here when we start decomposing our `Dynamic`s.

Imagine that we constructed a `Dynamic` that keeps track of a pair of `Colour`s:
```haskell
dynPair :: (Reflex t, MonadHold t m) 
        => Event t Colour
        -> Event t Colour
        -> m (Dynamic  t (Colour, Colour))
dynPair eInput1 eInput2 = do
  dColour1 <- holdDyn Blue eInput1
  dColour2 <- holdDyn Blue eInput2
  pure $ (,) <$> dColour1 <*> dColour2
 
```
and in some other part of our application we would like to break that pair apart into a pair of `Dynamic`s:
```haskell
splitPair :: Reflex t 
          => Dynamic t (Colour, Colour)
          -> (Dynamic t Colour, Dynamic t Colour)
splitPair dPair =
  let
    p1 = fmap fst dPair
    p2 = fmap snd dPair
  in
    (p1, p2)
```

This probably won't do what we want.

Imagine that the first `Event` passed to `dynPair` never fires.
Whenever the second `Event` passed to `dynPair` fires, the output `Dynamic` will update.
If that output is passed through `splitPair` we'll have a pair of `Dynamic`s that are updating, although the first of them will have `Event` firing that don't correspond to a change in state.

This is particularly problematic if we're trying to minimize the number of times we have to update a DOM tree.

We can see that in action here if we click back and forth between "Red" and "Blue" for one set of inputs:
<div id="basics-dynamic-split-1"></div>

We can solve this by using `holdUniqDyn`:
```haskell
holdUniqDyn :: (Reflex t, MonadHold t m, MonadFix m, Eq a) 
            => Dynamic t a 
            -> m (Dynamic t a)
```
to create a new version of `splitPair`:
```haskell
splitPair :: (Reflex t, MonadHold t m, MonadFix m)
          => Dynamic t (Colour, Colour)
          -> m (Dynamic t Colour, Dynamic t Colour)
splitPair dPair =
  do
    p1 <- holdUniqDyn (fmap fst dPair)
    p2 <- holdUniqDyn (fmap snd dPair)
    pure (p1, p2)
```

Which does what we want with respect to minimizing unnecessary updates:
<div id="basics-dynamic-split-2"></div>

This highlights an additional issue with our implementation of `dynPair` - if we clicked the same button over and over, we'd trigger updates even when the state wasn't changing.
If this was important to us we could address this by using `holdUniqDyn` within `dynPair` itself.

## Next up

We now have all the pieces that we need to build an FRP network.

In the [next post](../dom/) we'll start looking at how to create DOM elements using `reflex-dom`.
