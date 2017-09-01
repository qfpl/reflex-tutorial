---
title: Events
date: 2017-09-02
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

<div id="grid-setup"></div>

## What is an `Event`?

An `Event` in `reflex` looks like this:

```haskell
data Event t a
```

and has values of type `a` at particular points in time.

```
[(t, a)] 
```

The observable points of time are also known as frames, 


Every external triggered event happens in its own frame


So we're dealing with something like event-sourcing ...
... if it was created by mathematicians instead of consultants


```haskell
eOutput = eInput
```

<div id="basics-events-frame"></div>

<div id="basics-events-tick"></div>

## Building `Event`s

`Event`s derived from other `Event`s can occur in the same frame.

There is an obvious way that this can happen


```haskell
instance Functor (Event t) where ...
```

```haskell
flipColour :: Colour -> Colour
flipColour Red  = Blue
flipColour Blue = Red
```

```haskell
eOutput = flipColour <$> eInput
```

<div id="basics-events-flipper"></div>


We tend to use `<$` often in FRP

```haskell
(<$) :: Functor f => a -> f b -> f a
```

```haskell
eOutput = Blue <$ eInput
```

<div id="basics-events-blue"></div>

We can create an `Event` which fires in _some_ of the frames that another `Event` is firing in.

The simplest way to do that is to filter an `Event` with a predicate:

```haskell
ffilter :: Reflex t
        => (a -> Bool) 
        -> Event t a 
        -> Event t a
```

```haskell
isRed :: Colour -> Bool
isRed Red  = True
isRed Blue = False
```

```haskell
eOutput = ffilter isRed eInput
```

<div id="basics-events-red"></div>

```haskell
fmapMaybe :: Reflex t 
          => (a -> Maybe b) 
          -> Event t a 
          -> Event t b
```

```haskell
parseColour :: Text -> Maybe Colour
parseColour "Red"  = Just Red
parseColour "Blue" = Just Blue
parseColour _      = Nothing
```

```haskell
eOutput = fmapMaybe parseColour eInput
```

<div id="basics-events-parse"></div>

```haskell
fmapMaybe    :: Reflex t 
             => (a -> Maybe b)
             -> Event t a
             -> Event t b
```

```haskell
fmapMaybe id :: Reflex t 
             => 
                Event t (Maybe c) 
             -> Event t c
```

```haskell
fanEither :: Reflex t 
          => Event t (Either a b) 
          -> (Event t a, Event t b)
```

```haskell
splitColour :: Colour -> Either () ()
splitColour Red  = Left ()
splitColour Blue = Right ()
```

```haskell
(eLeft, eRight) = fanEither (splitColour <$> eInput)
```

<div id="basics-events-either"></div>

## When `Event`s collide

We can create `Event`s that might be occurring in the same frame as other `Event`s

That means when we want to work with multiple `Event`s, we need to be able to handle the case where several `Event`s are active in the same frame.

We're going to explore that by working on something that you might have come across this before as part of the game (or programming problem) known as [FizzBuzz](https://en.wikipedia.org/wiki/Fizz_buzz).
If you haven't come across it before then it would be worth taking a look before we continue.

Assume we have access to
```haskell
eCount :: Event t Int
```
which is increasing over time, and we have these function lying around:
```haskell
div3 :: Int -> Bool
div3 x = 
  x `mod` 3 == 0

div5 :: Int -> Bool
div5 x = 
  x `mod` 5 == 0
```

Then
```haskell
eFizz :: Event t Text
eFizz = 
  "Fizz" <$ ffilter div3 eCount
```
and
```haskell
eBuzz :: Event t Text
eBuzz = 
  "Buzz" <$ ffilter div5 eCount
```
will occasionally collide.

You can see that if you bump `eCount` up far enough:
<div id="basics-events-fizz-and-buzz"></div>

What can we do about the collisions?

The first thing that we can do is to use the `leftmost` function to assign priorities to `Event`s in the case that they fire simultaneously:
```haskell
leftmost :: [Event t a] -> Event t a
```

When multiple `Event`s in the list are firing in the same frame, the output of the `leftmost` function will fire using the `Event` nearest to the start of the list.

We can see an example of that when we introduce `eLeft`:
```haskell
eLeft :: Event t Text
eLeft = 
  leftmost [eFizz, eBuzz]
```

If we bump the counter to `3` then `eFizz` will fire with the value "Fizz" and so will `eLeft`.
If we bump the counter to `5` then `eBuzz` will fire with the value "Buzz" and so will `eLeft`.

If we bump the counter to `15`, then what will happen?
<div id="basics-events-leftmost"></div>

The `eFizz` `Event` will fire with the value "Fizz" and the `eBuzz` `Event` will fire with the value "Buzz".

As a result `eLeft` will fire with the value "Fizz", since `eFizz` was closer to the left of the list that was passed to `leftmost` than `eBuzz`.

In this particular case it won't help us much, but we've already been using `leftmost` in some of our earlier examples.
When we've seen `eInput` before, it has been built from `Event`s that fire when the "Red" and "Blue" buttons are pressed using leftmost:
```haskell
eInput = 
  leftmost [Red <$ eRed, Blue <$ eBlue]
```

That's still not quite what we want for the collisions while we're trying to solve the FizzBuzz problem.

We need something a bit more flexible, like the `mergeWith` function:
```haskell
mergeWith :: (a -> a -> a) -> [Event t a] -> Event t a
```

This let's us specify a function to use to combine the values from `Event`s which are firing simultaneously:
```haskell
eMerge :: Event t Text
eMerge = 
  mergeWith (<>) [eFizz, eBuzz]
```
and that seems to work for us when the `Event`s collide:
<div id="basics-events-mergeWith"></div>

We can be a little more concise than that though, since `reflex` adds a lot of useful typeclass instances for us.

As an example, we can use this:
```haskell
instance Semigroup a => Semigroup (Event t a) where ...
```
to rewrite `eMerge`:
```haskell
eMerge :: Event t Text
eMerge = 
  eFizz <> eBuzz
```
with no change to the meaning of our program.

We still need to print the numbers if we don't have a "Fizz" or a "Buzz", but we have the tools to fix that now.
We are making a prioritized choice between the `Event` with the word outputs and the `Event` with the number outputs:
```haskell
eMerge :: Event t Text
eMerge = 
  eFizz <> eBuzz

eCountText :: Event t Text
eCountText = 
  (Text.pack . show) <$> eCount

eFizzBuzz :: Event t Text
eFizzBuzz = 
  leftmost [eMerge , eCountText]
```

<div id="basics-events-fizzbuzz"></div>

## The advantages of semantics-driven abstractions


## Up next

`Event`s are only half of the FRP picture.

In the next post, we'll look at their partner - [`Behavior`s](../behaviors/)
