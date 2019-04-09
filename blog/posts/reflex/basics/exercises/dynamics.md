---
title: "Reflex exercises: Dynamics"
date: 2017-09-26
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

If you haven't done it already, I'd work through [the `Behavior` exercises](../behaviors/), since these build from there.

None of these exercises will change the overall behavior of our application.
Instead they'll expose more of the application that was previously being taken care of behind the scenes.

You'll probably want to copy pieces of your solutions from exercise to exercise as they progress.

## The first change

The first thing we're going to do is to change the `Behavior`s into `Dynamics`s in the `Inputs` data type:
```haskell
-- src/Ex05/Common.hs
data Inputs t =
  Inputs {
    idMoney    :: Dynamic t Money
  , idCarrot   :: Dynamic t Stock
  , idCelery   :: Dynamic t Stock
  , idCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }
```

Update your solution to reflect this change.

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex05.sh
```
from the `exercises` directory and then visit `http://localhost:9090` in your browser (although there is currently a bug in jsaddle-warp effecting Firefox).

It should update the browser every time that you save your code while it is in a compilable state.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex05"></div>

which should be behaving the same way as everything else on this page.

### Hints

You are given `Dynamic`s and you should already have code that works with `Behavior`s.
You've got this.

There is a solution in `src/Ex05/Solution.hs`, which you should look at when you're done or if you give up.

## The second change

The next change is to start producing some of the `Dynamic`s that are used in display.

The `Outputs` data type now includes two new `Dynamics`:
```haskell
-- src/Ex06/Common.hs
data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  , odChange :: Dynamic t Money
  , odVend   :: Dynamic t Text
  }
```

The `odChange` `Dynamic` will be used to display the change issued from the machine, which will be cleared back to zero whenever anything else happens.

The `odVend` `Dynamic` will show the name of the product that is vended in the event of a successful sale, will show an error message if an error occurred, and will be cleared back to an empty string whenever anything else happens.

There is a helper function available to generate error messages:
```haskell
-- src/Ex06/Common.hs
errorText :: Error -> Text
```

Update your solution to reflect this change.

As an optional extra exercise, you could write the code to create these `Dynamic`s with new top-level functions.

If you do this: which style do you prefer?
Lots of functions, or inlining everything into one big function?

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex06.sh
```
from the `exercises` directory.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex06"></div>

which should be behaving the same way as everything else on this page.

### Hints

Both of the `Dynamic`s that you need to produce will hold onto the last value that they've seen.

There is a solution in `src/Ex06/Solution.hs`, which you should look at when you're done or if you give up.

## The third change

The next change is to add the code that manages the amount of money in the machine.

We add in an `Event` which fires when the customer adds a dollar to the machine:
```haskell
data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  , ieAdd      :: Event t ()
  , ieBuy      :: Event t ()
  , ieRefund   :: Event t ()
  }
```
and we have moved the `Dynamic` tracking the amount of money in the machine from the `Inputs` to the `Outputs`:
```haskell
data Outputs t =
  Outputs {
    oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t Error
  , odMoney  :: Dynamic t Money
  , odChange :: Dynamic t Money
  , odVend   :: Dynamic t Text
  }
```

Update your solution to reflect this change.

Separate the code for managing the money out into it's own function.
Make sure that the amount of money can never go negative.

Separating out the function is only one step along the separation-of-concerns axis.
We could go further and create a `MoneyInputs` type to gather the `Event`s that are used as inputs to the function.

Give that a go if it sounds interesting to you.
What do you think of it?
Is being that explicit worth the extra typing for you, or would you rather just solve problems of this size inline and move on?

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex07.sh
```
from the `exercises` directory.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex07"></div>

which should be behaving the same way as everything else on this page.

### Hints

There are three `Event`s which will effect the amount of money in the machine.
Make sure that you deal with them all.

There is a solution in `src/Ex07/Solution.hs`, which you should look at when you're done or if you give up.

## The fourth change

You now need to fill out the body of the function which creates the `Dynamic`s for the items in stock:
```haskell
-- src/Ex08/Exercise.hs
mkStock ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
```

The function takes an initial stock level, the product to stock, and an `Event` which fires with the name of the product when a product is vended.
Make sure that you will never end up with a negative quantity of stock.

In case a reminder is helpful, the `Product` and `Stock` data types look like this:
```haskell
-- src/Ex08/Common.hs
data Product =
  Product {
    pName :: Text
  , pCost :: Money
  }

data Stock =
  Stock {
    sProduct  :: Product
  , sQuantity :: Int
  }
```

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex08.sh
```
from the `exercises` directory.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex08"></div>

which should be behaving the same way as everything else on this page.

### Hints

You should have all of the pieces you need for this by now, although it might take more `Event`-wrangling than the previous exercises on this page.

There is a solution in `src/Ex08/Solution.hs`, which you should look at when you're done or if you give up.
