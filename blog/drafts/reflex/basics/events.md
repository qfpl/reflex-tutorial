---
title: Events
date: 2017-09-02
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js, /js/reflex/basics/rx.all.min.js, /js/reflex/basics/rxjs-example.js
---

<div id="grid-setup"></div>

[Previously](../introduction/) we introduced FRP and the `reflex` library, and now we're going to take a look at the first of the main types that is used in these kinds of systems.

## What is an `Event`?

An `Event` is used to represent something that has values of a particular type at various instants of time.

An `Event` in `reflex` looks like this:

```haskell
data Event t a
```

and we can think of it as being a bit like a list of pairs of times and values:

```
[(t, a)] 
```

where the time is always increasing.

(We will ignore the `t` parameter for quite a while, but it is for keeping track of which "timeline" we are working in)

If we have a button on a webpage, we'll likely have
```
eClicked :: Event t ()
```
which will fire every time the button is pressed.

Similarly we could have
```
eChanged :: Event t Text
```
giving us the current value of a text input every time it is altered.

In both cases we have a single `Event`, but it could fire many times.
This is likely to be pretty different to what you are used to if you are use to an event-handling system where you need to poll for new events, or where you have to set up or otherwise deal with an event loop.

These points in time aren't measured in sections, but in actions.
If an externally triggered `Event` happens, a new logical point in time is created for that `Event`, and no other externally triggered `Event`s will be firing at the point in time.

These are the observable points of time of the system.
In `reflex` these are often referred to as _frames_.
The documentation around the `sodium` library sometimes refers to them as _transactions_.

If you're familiar with "event sourcing", this linearization of event occurrences might seem familiar. 

We can have multiple `Event`s happening in the same frame though, although only one of them will be happening due to something external to the system.

The simplest way that we can set up a derived `Event` is by defining a new `Event` based on another:

```haskell
eOutput :: Event t Colour
eOutput = eInput
```

We often refer to the combination of `Event`s and `Behavior`s as an FRP network, and this is about as simple as an FRP network gets.

In this case, the only externally triggered `Event`s are the button presses:
<div id="basics-events-frame"></div>

We can bring a timer into play, and since the timeout `Event`s are triggered from outside of the FRP network we can see that the timeout and button press `Event`s never occur in the same frame:
<div id="basics-events-tick"></div>

## Building `Event`s

We can create derived `Event`s in more exciting ways than just defining new `Event`s to be equal to existing `Event`s.

There is `Functor` instance for `Event`s:
```haskell
instance Reflex t => Functor (Event t) where ...
```

We can define a simple function on `Colour`s:
```haskell
flipColour :: Colour -> Colour
flipColour Red  = Blue
flipColour Blue = Red
```
and use that to write our first interesting FRP network:
```haskell
eOutput :: Event t Colour
eOutput = flipColour <$> eInput
```

<div id="basics-events-flipper"></div>

There is flipped version of `fmap` in `reflex`, called `ffor`:
```haskell
eOutput :: Event t Colour
eOutput = ffor eInput flipColour
```
which some people use with the `LambdaCase` language extension:
```haskell
{-# LANGUAGE LambdaCase #-}
eOutput :: Event t Colour
eOutput = ffor eInput $ \case
            Red -> Blue
            Blue -> Red
```
which you might prefer.

We'll see `<$` used often in FRP:
```haskell
(<$) :: Functor f => a -> f b -> f a
```
to map a constant value across a `Functor`.

In the case of `Event`s, it is like we are borrowing the points of time from the `[(t, b)]` interpretation, and then replacing all of the values of `b` with a constant value of type `a`.

In this case, we'll paint the world blue:
```haskell
eOutput :: Event t Colour
eOutput = Blue <$ eInput
```

<div id="basics-events-blue"></div>

Sometimes we want an `Event` which is like another `Event` but only fires when certain conditions hold.

We can create an `Event` which fires in _some_ of the frames that another `Event` is firing in.

The simplest way to do that is using `ffilter`:
```haskell
ffilter :: Reflex t
        => (a -> Bool) 
        -> Event t a 
        -> Event t a
```

We can write a simple predicate:
```haskell
isRed :: Colour -> Bool
isRed Red  = True
isRed Blue = False
```
and test is out:
```haskell
eOutput :: Event t Colour
eOutput = ffilter isRed eInput
```

<div id="basics-events-red"></div>

If we want to do some kind of transformation of an `Event` while filtering values, we can use `fmapMaybe`:
```haskell
fmapMaybe :: Reflex t 
          => (a -> Maybe b) 
          -> Event t a 
          -> Event t b
```

Here we'll write a dodgy parser:
```haskell
parseColour :: Text -> Maybe Colour
parseColour "Red"  = Just Red
parseColour "Blue" = Just Blue
parseColour _      = Nothing
```
and test it out:
```haskell
eOutput :: Event t Colour
eOutput = fmapMaybe parseColour eInput
```

<div id="basics-events-parse"></div>

A handy trick is that you can use `fmapMaybe id` to filter out any `Nothing` values from a `Event t (Maybe a)`:

```haskell
fmapMaybe id :: Reflex t 
             => Event t (Maybe a) 
             -> Event t a
```

We can also split out each side of an `Either` value:
```haskell
fanEither :: Reflex t 
          => Event t (Either a b) 
          -> (Event t a, Event t b)
```

I'm stretching a bit for good colour based examples:
```haskell
splitColour :: Colour -> Either () ()
splitColour Red  = Left ()
splitColour Blue = Right ()
```
but am happy to make do with what I have:
```haskell
eLeft, eRight :: Event t ()
(eLeft, eRight) = fanEither (splitColour <$> eInput)
```

<div id="basics-events-either"></div>

We can use `fmapMaybe` or `fanEither` to split apart our sum types and deal with each of the cases, which can be very useful.

There are some other tools in `reflex` for efficiently working with larger sum types that we'll cover later on, but we don't have to worry about them just yet.

## When `Event`s collide

We've already seen that we can create `Event`s that might be occurring in the same frame as other `Event`s

That means when we want to work with multiple `Event`s, we need to be able to handle the case where several of the `Event`s are firing in the same frame.

We're going to explore that by working on something that you might have come across this before as part of the game (or programming problem) known as [FizzBuzz](https://en.wikipedia.org/wiki/Fizz_buzz).
If you haven't come across it before then it is worth taking a look before we continue.

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
eFizz = "Fizz" <$ ffilter div3 eCount
```
and
```haskell
eBuzz :: Event t Text
eBuzz = "Buzz" <$ ffilter div5 eCount
```
will occasionally collide.

You can see that if you bump `eCount` up far enough:
<div id="basics-events-fizz-and-buzz"></div>

What can we do about the collisions?

The simplest thing that we can do is to use the `leftmost` function to assign priorities to `Event`s in the case that they fire simultaneously:
```haskell
leftmost :: [Event t a] -> Event t a
```

When multiple `Event`s in the list are firing in the same frame, the output of the `leftmost` function will fire using the `Event` nearest to the start of the list.

We can see an example of that when we introduce `eLeft`:
```haskell
eLeft :: Event t Text
eLeft = leftmost [eFizz, eBuzz]
```

If we bump the counter to `3` then `eFizz` will fire with the value "Fizz" and so will `eLeft`.
If we bump the counter to `5` then `eBuzz` will fire with the value "Buzz" and so will `eLeft`.

If we bump the counter to `15`, then what will happen?
<div id="basics-events-leftmost"></div>

The `eFizz` `Event` will fire with the value "Fizz" and the `eBuzz` `Event` will fire with the value "Buzz".

As a result `eLeft` will fire with the value "Fizz", since `eFizz` was closer to the left of the list that was passed to `leftmost` than `eBuzz`.

It isn't what we're looking for right now, but it's worth pointing out that we've already been using `leftmost` in some of our earlier examples.
When we've seen `eInput` before it has been built from `Event`s that fire when the "Red" and "Blue" buttons are pressed using leftmost:
```haskell
eInput = leftmost [Red <$ eRed, Blue <$ eBlue]
```

For the "FizzBuzz" collisions we need something a bit more flexible.
The `mergeWith` function:
```haskell
mergeWith :: (a -> a -> a) -> [Event t a] -> Event t a
```
is what we need.

It lets us specify a function to use to combine the values from `Event`s which are firing simultaneously.
The combination is effectively a `foldl1` over the `Event`s which are firing.

We will use `(<>)` to concatentate the `Text` in the `Event`s:
```haskell
eMerge :: Event t Text
eMerge = mergeWith (<>) [eFizz, eBuzz]
```
and that seems to work for us when the `Event`s collide:
<div id="basics-events-mergeWith"></div>

We can be more concise than that, as `reflex` adds a lot of useful typeclass instances for us.

This instance:
```haskell
instance Semigroup a => Semigroup (Event t a) where ...
```
can be used to simplify `eMerge`:
```haskell
eMerge :: Event t Text
eMerge = eFizz <> eBuzz
```
with no change to the meaning of our program.

We still need to print the numbers if we don't have a "Fizz" or a "Buzz", but we have the tools to fix that now.
We are making a prioritized choice between the `Event` with the word outputs and the `Event` with the number outputs:
```haskell
eMerge :: Event t Text
eMerge = eFizz <> eBuzz

eCountText :: Event t Text
eCountText = (Text.pack . show) <$> eCount

eFizzBuzz :: Event t Text
eFizzBuzz = leftmost [eMerge , eCountText]
```

<div id="basics-events-fizzbuzz"></div>

and that solves the "FizzBuzz" problem.

If we want to remove some simultaneously occurring `Event`s, we can do so with `difference`:
```haskell
difference :: Event t a -> Event t b -> Event t a
```

Although we need a more contrived solution to the "FizzBuzz" problem in order to demonstrate it:
```haskell
eMerge :: Event t Text
eMerge = eFizz <> eBuzz

eCountText :: Event t Text
eCountText = (Text.pack . show) <$> eCount

eDiff :: Event t Text
eDiff = difference eCountText eMerge

eFizzBuzz :: Event t Text
eFizzBuzz = leftmost [eDiff, eMerge]
```

<div id="basics-events-fizzbuzz-flip"></div>

## The advantages of semantics-driven abstractions

Why did `reflex` bother specifying how to deal with simultaneous `Event`s?
They did because it followed from the semantics.

Not everyone does things this way.

The Manning book on FRP uses something like this snippet of RxJS to demonstrate the problem.

```javascript
var button = document.getElementById("rxjs-button");
var clicks = Rx.Observable.fromEvent(button, "click");

var ones = clicks.scan(count => count + 1, 0);

var hundreds = ones.map(function(x) { return x * 100; });

var sum = ones.combineLatest(hundreds, function(o, h) {
    return o + h; 
  });

sum.subscribe(function(s) { alert(s); });
```

We want to increase a counter whenever a button is pressed.
We'll also multiply the click count `Event`, and then add those two `Event`s together.

Let's see how that goes, by clicking this two or three times:

<div class="panel panel-default panel-body">
  <button id="rxjs-button" class="btn btn-default">Click Me</button>
</div>

Whoops!  We're seeing intermediate state - something referred to as "glitching" - where we didn't expect it.

The equivalent `reflex` code looks like this:
```haskell
  button <- getElementById "reflex-button"
  eClick <- domEvent Click button

  eOnes <- accum (+) 0 (1 <$ eClick)

  let eHundreds = (* 100) <$> eOnes

  let eSum = mergeWith (+) [eOnes, eHundreds]

  alertEvent show eSum
```
although it is using some things we haven't introduced yet.

It behaves as we would expect:
<div id="basics-events-clickMe"></div>

There may be ways to patch up the RxJS code, and maybe they have already happened.
The RxJS library isn't designed starting with semantics though, and so the risk is that there are more glitches waiting to be found, and that patching could go on for a long time.

## Next up

`Event`s are only half of the FRP picture.

In the [next post](../behaviors/) we'll look at their partner - `Behavior`s.
