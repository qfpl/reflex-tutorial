---
title: Dynamics
date: 2017-09-04
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

```haskell
data Dynamic t a
```

```haskell
(Event t a, Behavior t a) 
```


```haskell
updated :: Reflex t 
        => Dynamic t a 
        -> Event t a
```

```haskell
current :: Reflex t 
        => Dynamic t a 
        -> Behavior t a
```


It manages state, and lets you know when the state has changed ...

... without having to poll the state for changes


We use `Dynamic` for the things that we want to change


Because `Dynamic`s are values we can pass them down through other components, right to where they need to be


We construct `Dynamic`s directly rather than combining `Event`s and `Behavior`s for reasons of correctness and efficiency


```haskell
holdDyn     :: (Reflex t, MonadHold t m) 
            => a 
            -> Event t a 
            -> m (Dynamic  t a)
```

```haskell
foldDyn     :: (Reflex t, MonadHold t m, MonadFix m) 
            => (a -> b -> b) 
            -> b 
            -> Event t a 
            -> m (Dynamic  t b)
```


```haskell
foldDyn ($) :: (Reflex t, MonadHold t m, MonadFix m) 
            => c 
            -> Event t (c -> c)
            -> m (Dynamic  t c)
```


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

##

```haskell
instance Reflex t => Functor     (Dynamic t) where ...
instance Reflex t => Applicative (Dynamic t) where ...
instance Reflex t => Monad       (Dynamic t) where ...
```

##

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

##

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

##

<!--There are also tools available for deconstructing them:

##

It can be helpful to use this while pulling things apart:-->
```haskell
holdUniqDyn :: (Reflex t, MonadHold t m, MonadFix m, Eq a) 
            => Dynamic t a 
            -> m (Dynamic t a)
```

<!--
. . .

It removes `Event` firings when the value hasn't changed.
-->


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
