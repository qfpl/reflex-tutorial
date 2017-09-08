---
title: Dynamics
date: 2017-09-04
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

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

TODO usefulness for updating a DOM

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

There is a variant that lets us fold a function through a series of `Event` firings:
```haskell
foldDyn     :: (Reflex t, MonadHold t m, MonadFix m) 
            => (a -> b -> b) 
            -> b 
            -> Event t a 
            -> m (Dynamic t b)
```
which we'll be using a lot with the function application operator - `($)`:
```haskell
foldDyn ($) :: (Reflex t, MonadHold t m, MonadFix m) 
            => c 
            -> Event t (c -> c)
            -> m (Dynamic t c)
```

## An example of using `Dynamic`s

```haskell
counter :: (Reflex t, MonadHold t m, MonadFix m) 
        => Event t ()
        -> m (Dynamic t Int)
counter eAdd        =
  foldDyn ($) 0 $ 
    (+ 1)   <$ eAdd
```


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

<div id="basics-dynamic-counter"></div>


## Removing extraneous updates

```haskell
dynPair :: (Reflex t, MonadHold t m) 
        => Event t Colour
        -> Event t Colour
        -> m (Dynamic  t (Colour, Colour))
dynPair eInput1 eInput2 = do
  dColour1 <- holdDyn Blue eInput1
  dColour2 <- holdDyn Blue eInput2
  pure $      (,) <$> dColour1 <*> dColour2
 
```

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

```haskell
holdUniqDyn :: (Reflex t, MonadHold t m, MonadFix m, Eq a) 
            => Dynamic t a 
            -> m (Dynamic t a)
```

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

## Bringing `RecursiveDo` into the picture

`Behavior`s are specified at all points of time

 This is also true of `Dynamic`s

`Event`s are only specified at some unique points of time

So all `Behavior`s have a value before any of the `Event`s fire

It is perfectly valid to have `Event`s that build a `Behavior` and also depend on the `Behavior`

This is where the one frame delay comes in - we are using the old value of the `Behavior` to build the new value of the `Behavior`

This means some dependencies can be loops, which is fine

We just need a way to specify them

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  pure dCount
```


<div id="basics-recursiveDo-3"></div>

The limit could be coming from something like this:
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

and if you play around with these two widgets, you will see that they are linked.

TODO

```haskell
data Settings =
  Settings {
    settingLimit :: Int
  , settingStep  :: Int
  }
```

```haskell
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Settings
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dSettings eAdd eClear = mdo
  let dLimit      = settingsLimit <$> dSettings
      dStep       = settingsStep  <$> dSettings
      check c s l = c + s <= l
      dLimitOK    = check <$> dCount <*> dStep <*> dLimit
      eAddOK      = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+  )   <$> tag (current dStep) eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```


```haskell
counter (Settings <$> dLimit <*> dStep) eAdd eClear
```

<div id="basics-recursiveDo-4"></div>

## Next up

