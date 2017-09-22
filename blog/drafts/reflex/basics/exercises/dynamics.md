---
title: "Reflex exercises: Dynamics"
date: 2017-09-26
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

If you haven't done it already, I'd work through [the `Behavior` exercises](../behaviors/), since these builds from there.

None of these exercises will change the overall behavior of our application.
Instead they'll expose more the application, that was previously being taken care of behind the scenes

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
from the `exercises` directory.

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

The `odChange` `Dynamic` will show the change issued if change is issued, and will be cleared back to zero whenever anything else happens.

The `odVend` `Dynamic` will show the name of the product that is vended in the event of a successful sale, will show an error message if an error occurred, and will be cleared back to an empty string whenever anything else happens.

There is a helper function available to generate error messages:
```haskell
-- src/Ex06/Common.hs
errorText :: Error -> Text
```

Update your solution to reflect this change.

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

TODO optional extra
TODO - separate out the code that builds these dynamics into functions

There is a solution in `src/Ex06/Solution.hs`, which you should look at when you're done or if you give up.

## The third change

TODO

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

TODO optional extra
TODO - gate the network so it never goes negative
TODO - separate it out into another function

There is a solution in `src/Ex07/Solution.hs`, which you should look at when you're done or if you give up.

## The fourth change

TODO

Update your solution to reflect this change.

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

TODO

There is a solution in `src/Ex08/Solution.hs`, which you should look at when you're done or if you give up.
