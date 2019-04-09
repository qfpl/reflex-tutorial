---
title: "Reflex exercises: Events"
date: 2017-09-21
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

If you haven't done it already, I'd work through [the first exercise](../introduction/) in order to get everything set up and working.

## The problem

We're going to start to build a HTML vending machine.

It's a very capitalist goal, so we'll start with a type alias to focus ourselves:
```haskell
-- src/Ex01/Common.hs
type Money = Int
```

We're going to need to track what items we have for sale and what they cost:
```haskell
-- src/Ex01/Common.hs
data Product =
  Product {
    pName :: Text
  , pCost :: Money
  }
```
and we'll set up some products based on the fact that we're aiming our vending machine at the healthy eaters out there:
```haskell
-- src/Ex01/Common.hs
carrot :: Product
carrot = Product "Carrot" 1

celery :: Product
celery = Product "Celery" 2

cucumber :: Product
cucumber = Product "Cucumber" 3
```

We have inputs for the buttons on the machine corresponding to our products, and for the "Refund" button:
```haskell
-- src/Ex01/Common.hs
data Inputs t =
  Inputs {
    ieCarrot   :: Event t ()
  , ieCelery   :: Event t ()
  , ieCucumber :: Event t ()
  , ieRefund   :: Event t ()
  }
```
and we have outputs for the product to put in the tray, the amount of money that purchase consumes, the change to issue, and whether someone tried to purchase something they couldn't afford:
```haskell
-- src/Ex01/Common.hs
data Outputs t =
  Outputs {
    oeVend           :: Event t Text
  , oeSpend          :: Event t Money
  , oeChange         :: Event t Money
  , oeNotEnoughMoney :: Event t ()
  }
```

You are given a function to fill in that takes the amount of money in the system and a value of type `Inputs`, and produces a value of type `Outputs`:
```haskell
-- src/Ex01/Exercise.hs
ex01 :: Reflex t 
     => Money 
     -> Inputs t 
     -> Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    eVend =
      never
    eSpend =
      never
    eChange =
      never
    eNotEnoughMoney =
      never
  in
    Outputs eVend eSpend eChange eNotEnoughMoney
```

To see what this looks like in the context of our HTML vending machine, run:
```
> nix-shell
nix-shell> ./ex01.sh
```
from the `exercises` directory and then visit `http://localhost:9090` in your browser (although there is currently a bug in jsaddle-warp effecting Firefox).

It should update the browser every time that you save your code while it is in a compilable state.

## The goal

### The specification

Here are the things we are after.

When the `Event` is triggered for a product:

- if there is enough money in the system, `eVend` should fire with the name of the product and `eSpend` should fire with the cost of the product.
- if there is not enough money in the system, `eNotEnoughMoney` should fire

When `eRefund` is triggered:

- `eChange` should fire with the amount of money that is in the system

Everything else is taken care of for you.

Run the script:
```
> nix-shell
nix-shell> ./ex01.sh
```
open `src/Ex01/Exercise.hs` in an editor and give it a try.

### What it should look like

If you achieve those things, you should get something that behaves like this:

<div id="ex01"></div>

### Hints

A single `Event t Product` would be much easier to deal with than the trio of `eCarrot` , `eCelery` and `eCucumber`.

There is a solution in `src/Ex01/Solution.hs`, but it's worth spending a little while on this if you get stuck before you have a look.
