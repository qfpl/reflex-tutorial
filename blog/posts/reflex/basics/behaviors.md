---
title: Behaviors
date: 2017-09-03
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

<div id="grid-setup"></div>

[Previously](../events) we had a look at `Event`s, which describe values occurring at particular instants in time.
Now we'll have a look at the other half of the core FRP types - `Behavior`s.

## What is a `Behavior`?

```haskell
data Behavior t a
```

```haskell
t -> a
```

## Building `Behavior`s

```haskell
hold :: MonadHold t m
     => a
     -> Event t a
     -> m (Behavior t a)
```

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
 
```

```haskell
tag :: Reflex t 
    => Behavior t a
    -> Event t b
    -> Event t a
```

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  pure $ tag bColour eSample
```

<div id="basics-behaviors-sampleBlue1"></div>


The state doesn't change until the frame _after_ the firing of the `Event`s in `hold`.
We can see that by sampling from the `Behavior` when any of the buttons are pressed


```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  pure $ tag bColour eAny
```

<div id="basics-behaviors-sampleBlue2"></div>


```haskell
attach :: Reflex t 
       => Behavior t a 
       -> Event t b 
       -> Event t (a, b)
```

```haskell
attachWith :: Reflex t 
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
instance Functor (Behavior t) where ..
```

```haskell
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput
  let bFlippedColour = flipColour <$> bColour
  pure $ tag bFlippedColour eSample
```

<div id="basics-behaviors-sampleFlipBlue"></div>

```haskell
instance Applicative (Behavior t) where ..
```

```haskell
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

This combined type is called a [`Dynamic`](../dynamics/)
