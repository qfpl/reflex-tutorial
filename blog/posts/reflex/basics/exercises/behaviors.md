---
title: "Reflex exercises: Behaviors"
date: 2017-09-23
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

If you haven't done it already, I'd work through [the `Event` exercise](../events/), since this builds on it.

## The first change

We're going to start off with a small change.

We're going to take the amount of money in the machine as a `Behavior` and make it part of the `Inputs` type:
```haskell
-- src/Ex02/Common.hs
data Inputs t =
  Inputs {
    ibMoney    :: Behavior t Money
  , ieCarrot   :: Event t ()
  , ieCelery   :: Event t ()
  , ieCucumber :: Event t ()
  , ieRefund   :: Event t ()
  }
```
and we're going to use a more descriptive error type:
```haskell
-- src/Ex02/Common.hs
data Error =
    NotEnoughMoney
  deriving (Eq, Ord, Show)
```
as part of the output:
```haskell
-- src/Ex02/Common.hs
data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  }
```

Update your solution to the previous exercise so that it takes into account the changes to the inputs and outputs.

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex02.sh
```
from the `exercises` directory and then visit `http://localhost:8080` in your browser (although there is currently a bug in jsaddle-warp effecting Firefox).

It should update the browser every time that you save your code while it is in a compilable state.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex02"></div>

which will bear an uncanny resemblance to the result of the first exercise.

### Hints

You shouldn't need to use any of the typeclass instances to do this.
If you end up using `hold` then you have gone too far and should turn back.

There is a solution in `src/Ex02/Solution.hs`, which you should look at when you're done or if you give up.

## The second change

We're going to switch from having a "Buy" button for each product to a single "Buy" button and a group of radio buttons to handle the selection.

The selection will be given to you as a `Behavior t Text` containing the name of the selected product:
```haskell
-- src/Ex03/Common.hs
data Inputs t =
  Inputs {
    ibMoney    :: Behavior t Money
  , ibSelected :: Behavior t Text
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }
```

Update your solution to reflect this change.

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex03.sh
```
from the `exercises` directory.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex03"></div>

### Hints

If you can get to an `Event t Product` which fires when the customer hits "Buy", then the rest of the program should be unchanged from the previous solution.

You have `Map` from the `containers` library in your import list.
This is a golden opportunity to do things with `sequence` and with the `Applicative`-interface helpers.

Remember that `fmapMaybe id` is very handy.

There is a solution in `src/Ex03/Solution.hs`, which you should look at when you're done or if you give up.

## The third change

We're going to start tracking the stock levels in the vending machine.

We introduce a type for this:
```haskell
-- src/Ex04/Common.hs
data Stock =
  Stock {
    sProduct  :: Product
  , sQuantity :: Int
  } deriving (Eq, Ord, Show)
```
and we will pass in the stock levels as `Behavior`s:
```haskell
-- src/Ex04/Common.hs
data Inputs t =
  Inputs {
    ibMoney    :: Behavior t Money
  , ibCarrot   :: Behavior t Stock
  , ibCelery   :: Behavior t Stock
  , ibCucumber :: Behavior t Stock
  , ibSelected :: Behavior t Text
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }
```

The output is unchanged, but we have a new error condition to detect:
```haskell
-- src/Ex04/Common.hs
data Error =
    NotEnoughMoney
  | ItemOutOfStock
  deriving (Eq, Ord, Show)
```

Update your solution to reflect this change.

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex04.sh
```
from the `exercises` directory.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex04"></div>

### Hints

The biggest challenge here will be getting the inputs into position.
It's similar to the last exercise, but different enough to be fiddly.

Remember to check the handling of the new error condition properly.

There is a solution in `src/Ex04/Solution.hs`, which you should look at when you're done or if you give up.
