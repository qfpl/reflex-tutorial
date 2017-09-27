---
title: "Reflex exercises: DOM"
date: 2017-09-27
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

## The first change

```haskell
data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  }
```

```haskell
ex09 :: MonadWidget t m 
     => Inputs t 
     -> m (Event t Text)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  let
    eVend =
      never

  pure eVend
```

```haskell
moneyDisplay :: Money 
             -> Text
```

```haskell
buyRow :: MonadWidget t m 
       => m (Event t ())
buyRow =
  el "tr" $ do
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      button "Buy"
```

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex09"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

There is a solution in `src/Ex09/Solution.hs`, which you should look at when you're done or if you give up.

## The second change

We're going to change the layout a little, by using the Bootstrap library from Twitter.

Instead of making that change directly, write two functions:
```haskell
grid :: MonadWidget t m
     => m a 
     -> m a
grid = 
  el "table"

row :: MonadWidget t m
    => m a
    -> m b
    -> m c
    -> m d
    -> m d
row = 
  _ -- write this function
```
and refactor your code to make use of `row`.

For example, `buyRow` will now look like this:
```haskell
buyRow :: MonadWidget t m 
       => m (Event t ())
buyRow =
  let
    rBlank = pure ()
    r4     = button "Buy"
  in
    row rBlank rBlank rBlank r4
```

Now change `grid` and `row` to make use of Bootstrap.

This is the change to `grid`:
```haskell
grid :: MonadWidget t m
     => m a 
     -> m a
grid = 
  elClass "div" "container"
```

The new version of `buyRow` would look like this if we hadn't written `row`:
```haskell
buyRow :: MonadWidget t m 
       => m (Event t ())
buyRow =
  elClass "div" "row" $ do
    elClass "div" "col-md-3" $
      pure ()
    elClass "div" "col-md-1" $
      pure ()
    elClass "div" "col-md-1" $
      pure ()
    elClass "div" "col-md-1" $
      button "Buy"
```

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex10"></div>

which should be similar to what we had before, but styled closer to what we had before.

### Hints

There is a solution in `src/Ex10/Solution.hs`, which you should look at when you're done or if you give up.

## The third change

TODO checkbox

```haskell
stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
```

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex11"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

There is a solution in `src/Ex11/Solution.hs`, which you should look at when you're done or if you give up.

## The fourth change

TODO close the loop

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex12"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

There is a solution in `src/Ex12/Solution.hs`, which you should look at when you're done or if you give up.

## The fifth change

TODO radio button
TODO button with class

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex13"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

There is a solution in `src/Ex13/Solution.hs`, which you should look at when you're done or if you give up.

## The sixth change

TODO new part of app for managing stock, tracking amount of money collected and clearing it out

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex14"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

There is a solution in `src/Ex14/Solution.hs`, which you should look at when you're done or if you give up.
