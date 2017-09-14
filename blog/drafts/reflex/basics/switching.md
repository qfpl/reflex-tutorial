---
title: Switching
date: 2017-09-06
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

[Previously](../dom/) we looked at how to use `reflex-dom` to put DOM elements on a page.
Now we're going to look at how to modify our FRP network in response to user input.

## What is switching?

Everything we've done so far has involved building up an FRP network.
Sometimes we want to modify the FRP network in response to `Event`s.
That is what the various switching functions do.

There are two kinds of switching we can do:

- we can just rework the FRP network
- we can rework the FRP network while also modifying or removing some DOM elements

## Higher-order FRP

When we want to rework an FRP network, we usually do it via higher-order FRP.

We've seen higher-order functions: function which take functions as arguments, like:
```haskell
map   :: (a -> b) 
      -> [a] 
      -> [b]
```
or
```haskell
foldr :: (a -> b -> b) 
      -> b 
      -> [a] 
      -> b
```

Higher-order FRP involves an FRP type - one of `Event`, `Behavior` or `Dynamic` - which contains another FRP type.

Some examples turn up due to the fact that `Behavior` and `Dynamic` have `Monad` instances, giving rise to:
```haskell
join :: Behavior t (Behavior t a) 
     -> Behavior t a
```
and:
```haskell
join :: Dynamic  t (Dynamic  t a) 
     -> Dynamic  t a
```

There are other functions that also provide higher-order FRP functionality such as:
```haskell
switch        :: Behavior t (Event t    a)
              ->             Event t    a

switcher      :: (Reflex t, MonadHold t m) 
              =>             Behavior t a
              -> Event t    (Behavior t a)
              -> m          (Behavior t a)

switchPrompty :: (Reflex t, MonadHold t m) 
              =>             Event t    a
              -> Event t    (Event t    a)
              -> m          (Event t    a)
```
along with a few others.


It can be handy to think of a [railroad switch](https://en.wikipedia.org/wiki/Railroad_switch) while you're getting use to these methods.

We'll focus on `switch` for a while to motivate higher-order FRP:
```haskell
switch        :: Behavior t (Event t    a)
              ->             Event t    a
```

We can view a `Behavior t (Event t a)` as an `Event t a` which is varying over time.
There could be multiple sources of these `Event`s and the `Behavior` is being used to track which `Event` should be used at any given moment in time.
Since `Behavior`s have values at all points in time, that means there is always an `Event` which is selected.
The `switch` function is giving us access to the `Event` that is selected by the `Behavior`.


## Dynamic modifications to the DOM

### Faking it by hiding elements

### Switching out elements

TODO if we reroute something in a way that the network can detect is final, it can be garbage collected

### Using `Workflow`

## Next up
