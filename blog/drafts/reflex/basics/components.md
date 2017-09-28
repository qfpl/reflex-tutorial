---
title: Components
date: 2017-09-07
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

[Previously](../switching/) we looked at how to alter the FRP network in response to `Event`s.

Now we have the pieces we need to have a discussion about the ins and out of how to break apart those FRP networks into components that we can easily reason about and reuse.

We're going to iterate on our todo item from the post on working with the DOM, looking at a few different options and trade offs along the way.

## A remove button

```haskell
remove :: MonadWidget t m 
       => m (Event t ())
remove =
  button "Remove"
```

<div id="examples-component-remove"></div>

## A completion checkbox

```haskell
complete :: MonadWidget t m 
         => Bool 
         -> m (Event t Bool)
complete initial = do
  cb <- checkbox initial def

  pure $ cb ^. checkbox_change
```

<div id="examples-component-complete-basic"></div>


```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete

  let
    eChanges = updated dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges

  pure $ cb ^. checkbox_change
```

<!-- <div id="examples-component-complete-sample"></div>-->

```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        updated dComplete
      , current dComplete <@ ePostBuild
      ]

  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges

  pure $ cb ^. checkbox_change
```

<div id="examples-component-complete-postbuild"></div>

```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t (Bool -> Bool))
complete dComplete = do
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        updated dComplete
      , current dComplete <@ ePostBuild
      ]

  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges

  pure . fmap const $ cb ^. checkbox_change
```

<div id="examples-component-complete-endo"></div>

## The text of the item

```haskell
textRead :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t ())
textRead dText = do
  (e, _) <- el' "div" $
    dynText dText
  pure $ () <$ domEvent Dblclick e
```

<div id="examples-component-text-read"></div>

```haskell
textWrite :: MonadWidget t m 
          => Dynamic t Text 
          -> m (Event t Text, Event t ())
textWrite dText =
  _
```

<div id="examples-component-text-write"></div>

## Putting the item together

### When we need to build a model of the component

<div id="examples-component-todo-item"></div>

### Doing things a bit more efficiently

<div id="examples-component-todo-item-d"></div>

### When we don't need to build a model of a component

## Playing along at home

There are exercises [here](../exercises/components/) that will put your understanding of the above content to the test.
These exercises build up incrementally as the series progresses, so it would probably best to start the exercises beginning at the start of the series.

## Next up

In the [next post](../collections/) we'll look at how to manage collections in `reflex`.
