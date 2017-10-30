---
title: "Reflex exercises: DOM"
date: 2017-09-28
authors: dlaing
project: reflex
extra-js: /js/reflex/basics-exercises/solutions.min.js
---

If you haven't done it already, I'd work through [the `Dynamic` exercises](../dynamics/), since these build from there.

None of these exercises will change the overall behavior of our application, but they will result in some visual changes.

You'll probably want to copy pieces of your solutions from exercise to exercise as they progress.

## The first change

We're going to start displaying some of this information ourselves.

The input is going to become:
```haskell
-- src/Ex09/Common.hs
data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  }
```
and the output is going to become an `Event t Text` which fires with the name of the product whenever a sale is made.

The shape of the solution will be:
```haskell
-- src/Ex09/Exercise.hs
ex09 :: MonadWidget t m 
     => Inputs t 
     -> m (Event t Text)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  let
    eVend =
      never

  pure eVend
```

This widget will be laid out on the page inside of a `<table>` element, which you need to take into account.

You have access to a function which might be handy to format `Money` values:
```haskell
-- src/Ex09/Common.hs
moneyDisplay :: Money 
             -> Text
```

You can play with this in a browser by running:
```
> nix-shell
nix-shell> ./ex09.sh
```
from the `exercises` directory and then visit `http://localhost:8080` in your browser (although there is currently a bug in jsaddle-warp effecting Firefox).

It should update the browser every time that you save your code while it is in a compilable state.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex09"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

The row containing the "Buy" button might look something like this:
```haskell
-- src/Ex09/Exercise.hs
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

There is a solution in `src/Ex09/Solution.hs`, which you should look at when you're done or if you give up.

## The second change

We're going to change the layout a little, by using the Bootstrap library from Twitter.

Instead of making that change directly, write two functions:
```haskell
-- src/Ex10/Exercise.hs
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
-- src/Ex10/Exercise.hs
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
-- src/Ex10/Exercise.hs
grid :: MonadWidget t m
     => m a 
     -> m a
grid = 
  elClass "div" "container"
```

The new version of `buyRow` would look like this if we hadn't written `row`:
```haskell
-- src/Ex10/Exercise.hs
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

We're going to write the widget that display information about the stock in the machine:
```haskell
-- src/Ex11/Exercise.hs
stockWidget :: MonadWidget t m 
            => Dynamic t Stock  -- ^ the stock item to display
            -> Dynamic t Text   -- ^ the name of the currently selected stock item
            -> m (Event t Text) -- ^ fires with the name of the stock item when it is selected
```

You have access to this function:
```haskell
-- src/Ex11/Run.hs
radioCheckbox :: (MonadWidget t m , Eq a) 
              => Dynamic t a    -- ^ the value associated with this checkbox
              -> Dynamic t a    -- ^ the value associated with the currently selected checkbox
              -> m (Event t a)  -- ^ fires with the value associated with this checkbox when it is selected
```
which acts like a radio button.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex11"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

Remember that you still have `row`.

There is a solution in `src/Ex11/Solution.hs`, which you should look at when you're done or if you give up.

## The fourth change

The blog post covered working with checkboxes, so we will use them to write something like a radio button:
```haskell
-- src/Ex12/Exercise.hs
radioCheckbox :: (MonadWidget t m , Eq a) 
              => Dynamic t a    -- ^ the value associated with this checkbox
              -> Dynamic t a    -- ^ the value associated with the currently selected checkbox
              -> m (Event t a)  -- ^ fires with the value associated with this checkbox when it is selected
```

When your solution is working there should always be exactly one checkbox that is selected.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex12"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

You can use `getPostBuild` to get hold of an `Event` you can use to sample `dValue`.
This will be handy if you need an `Event` with the value of `dValue` at the moment the checkbox was laid on the page:
```haskell
-- src/Ex12/Exercise.hs
radioCheckbox :: (MonadWidget t m , Eq a) 
              => Dynamic t a 
              -> Dynamic t a 
              -> m (Event t a)
radioCheckbox dValue dSelected = do
  ePostBuild <- getPostBuild
  let 
    eIniital :: Event t a = current dValue <@ ePostBuild
  ...
```
although are quite a few different ways to approach this problem.

What happens if we use `sample . current $ dValue` to get hold of the initial value?

There is a solution in `src/Ex12/Solution.hs`, which you should look at when you're done or if you give up.

## The fifth change

Now we're going to construct a proper radio button:
```haskell
-- src/Ex13/Exercise.hs
radioButton :: (MonadWidget t m , Eq a) 
            => Text           -- ^ the name of the button group
            -> Dynamic t a    -- ^ the value associated with this button
            -> Dynamic t a    -- ^ the value associated with the currently selected button
            -> m (Event t a)  -- ^ fires with the value associated with this button when it is selected
```
and adapt our code to use it.

A radio button is an `input` element the following attributes:

- the `type` is `radio`
- there is a `name` attribute that has the same value for all buttons in the group
- there is a `checked` attribute (which has no value) present if and only if the button is selected

We want to return an `Event t a` when this button is clicked.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex13"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

This will use some of the tools we have for customizing elements.

There is a solution in `src/Ex13/Solution.hs`, which you should look at when you're done or if you give up.

## The sixth change

The time has come to tie it all together.

Use the pieces that we've built up to define:

```haskell
-- src/Ex14/Exercise.hs
ex14 :: MonadWidget t m
     => m ()
```

This is the vending machine application.

For bonus points, go through and style it and really make it your own.

### What it should look like

If you succeed, you should get something that behaves like this:

<div id="ex14"></div>

which should be similar to what we had before, but perhaps styled a little differently.

### Hints

You can use `mkStock` to create your `Dynamic t Stock`s given your vend event:
```haskell
mkStock :: (Reflex t  MonadHold t m, MonadFix m) 
        => Int 
        -> Product 
        -> Event t Text 
        -> m (Dynamic t Stock)
```

You can display the stock information on the page using `stockWidget`:
```haskell
stockWidget :: MonadWidget t m 
            => Dynamic t Stock 
            -> Dynamic t Text 
            -> m (Event t Text)
```
although you'll need to combine those selection `Event`s to come up with an appropriate `Dynamic t Text` to feed it.

Everything else should already be there.

There is a solution in `src/Ex14/Solution.hs`, which you should look at when you're done or if you give up.
