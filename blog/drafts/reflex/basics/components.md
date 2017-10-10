---
title: Components
date: 2017-10-03
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css, /css/reflex/basics/todo.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

[Previously](../switching/) we looked at how to alter the FRP network in response to `Event`s.

Now we have the pieces we need to have a discussion about the ins and out of how to break apart those FRP networks into components that we can easily reason about and reuse.

We're going to iterate on our todo list item from the post on working with the DOM.
Along with the todo items themselves, we'll also be creating:

- a checkbox with both tracks and can toggle whether all items are marked as complete or not
- a button which only appears when at least one item is complete, which will be used to remove all of the complete items

and so we'll build the todo item components with that functionality in mind.

We'll be looking at a few different options and trade offs along the way.
The two main questions that will drive these choices are:

- how much of the state of our todo list do we want to expose to the rest of our code?
- do we want our various sub-components to be reusable on their own?

The code will start out sharing all of the state of the todo list via a `Dynamic`, with sub-components that aren't all that reusable.

## Some general advice

To start with, we want to have our components take `Dynamic`s as inputs and have `Event`s as outputs.
The application makes some of its state available to our component via `Dynamics`s, and the component handle its own inputs in the context of that state and then lets the application know about changes that should be made and when via `Event`s.

We'll have other code that uses `holdDyn` and `foldDyn` to collect various `Event`s into the `Dynamic`s that model our state.
Sometimes this code will use TODO mutual recursion
Eventually we'll probably end up with code that does a little bit of both

## A remove button

We'll start with an easy part of our todo list item - the "Remove" button.

It has no state to take in, and it returns an `Event` that lets us know when we should remove the item:
```haskell
remove :: MonadWidget t m 
       => m (Event t ())
remove =
  button "Remove"
```

<div id="examples-component-remove"></div>

So far, so good.

## A completion checkbox

The checkbox that manages the completion state of the todo item is a little more complicated.

If todo item was the only thing that was going to change the completion status of an item, we could pass in an initial value for the completion status and get an `Event` back to let us know whenever the status changes:
```haskell
complete :: MonadWidget t m 
         => Bool 
         -> m (Event t Bool)
complete initial = do
  cb <- checkbox initial def

  pure $ cb ^. checkbox_change
```

<div id="examples-component-complete-basic"></div>

We're going to be adding the ability to mark all todo items as being complete or incomplete, and so we need to be able to handle external modifications to the completion status.

We'll start with our default advice, and use `Dynamic`s as inputs and `Event`s as outputs, leading to this type signature:
```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
```

From there we could get the initial value of the `Dynamic` when this widget is laid out on the page:
```haskell
  initial <- sample . current $ dComplete
```
configure the checkbox to update when the `Dynamic` changes:
```haskell
  let 
    eChanges = 
      updated dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges
```
and then return an `Event` which fires when the user changes the value of the checkbox:
```haskell
  pure $ cb ^. checkbox_change
```
giving us:
```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete

  pure $ cb ^. checkbox_change
```

There's a problem with this though.

If we write some example code that closes the loop:
```haskell
completeExample = mdo
  eComplete <- divClass "todo-item" $
    complete dComplete

  dComplete <- holdDyn False eComplete

  el "div" $
    display dComplete
```
we will end up with nothing displayed and this:
```
thread blocked indefinitely in an MVar operation
```
in our JavaScript console.

We have seen that we can make forward references between our `Event`s, `Behavior`s, and `Dynamic`s using `RecursiveDo`.
In the cases we have looked at, all of the pieces we are using are brought into existence in the same frame and connected to each other behind the scenes.

In the above example we are not so lucky.
We need to sample `dComplete` in order to get hold of the initial value of the checkbox, and from that we can render the checkbox.
The output `Event` from the checkbox won't exist until the checkbox is laid out on the page, and we're using that `Event` to build up `dComplete`.

We can't build `dComplete` until we have rendered the checkbox, and we can't render the checkbox until we have gotten hold of an initial value via `dComplete`.

Let's look at an alternative approach that avoids that problem.

We'll start with the same signature:
```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
```

We'll then get hold of an `Event` which fires when our component is laid out:
```haskell
  ePostBuild <- getPostBuild
```
and we'll use that to build an `Event` which fires when the `Dynamic` changes and also fires with the value of the `Dynamic` at the time this widget is laid out on the page:
```haskell
  let 
    eChanges = leftmost [
        updated dComplete
      , current dComplete <@ ePostBuild
      ]
```
after which we proceed as before, but with a dummy initial value:
```haskell
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges

  pure $ cb ^. checkbox_change
```

All together now:
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

When we have an initial value we can render widgets in a single frame, whereas if we use `getPostBuild` to set the initial values the widgets will be partly rendered in one frame and then updated with the initial values in a later frame.
We'll use `sample` when it is safe to do so and `getPostBuild` otherwise.

## The text of the item

We want the todo item to display as normal until the user double clicks on the text of the item.

When that happens, we want to have the text of the todo item presented in a text input for editing, and:

- if the user presses the "Enter" key while the text is not empty, the edit will be accepted.
- if the user presses the "Enter" key while the text is empty, the item will be deleted.
- if the user presses the "Escape" key then any edits should be ignored.

After any of the above happens, the widget should revert to the usual todo item display, possibly with some new text.

We're going to use a `Workflow` to handle jumping backwards and forwards between these two behaviors.

The approach we take will vary slightly depending on whether or not other parts of our code will be interested in the text of the todo items.
If they are, we will want to create a `Dynamic t Text` to track the text of the todo items so that we can pass it along.
If they are not, we can do things slightly differently and reduce the amount of state that our clients have to work with and reason about.

### When we want to create a `Dynamic t Text`

When we are displaying the text of the todo item, we just need to put the text in a `div` and pay attention to any double clicks:
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

When we are editing the text, we could use a component which takes a `Dynamic t Text` as input and fires an `Event t Text` to indicate that the text has changed:
```haskell
textWrite :: MonadWidget t m 
          => Dynamic t Text 
          -> m (Event t Text)
```

This would mean that we would have to write some text-specific FRP logic in our todo item component.
Whenever the text changes, we'll need to strip the text of whitespace and work out whether it is empty.
If it's empty, we should fire an `Event` to indicate that the item should be removed.
If it's not empty, we should update the text of the item.

Instead, we are going to deal with this inside of our text-focused sub-component, since that makes more sense to me.

This means that our component will return an additional `Event` that fires when the todo item should be removed:
```haskell
textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
textWrite dText = mdo
```

```haskell
  ePostBuild <- getPostBuild
```

```haskell
  let
    eChanges = leftmost [
        eDone
      , current dText <@ ePostBuild
      ]
```

```haskell
  ti <- textInput $
    def & textInputConfig_setValue .~
            eChanges
```

```haskell
  let
    dValue = ti ^. textInput_value
```

```haskell
    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress
    eEscape   = ffilter (isKey Escape) eKeypress
```

```haskell
    eAtEnter  = Text.strip <$> current dValue <@ eEnter
    eAtEscape =                current dText  <@ eEscape
```

```haskell
    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , eAtEscape
      ]
```

```haskell
    eRemove = () <$ ffilter Text.null eAtEnter
```

```haskell
  pure (eDone, eRemove)
```


```haskell
textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
textWrite dText = mdo
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        eDone
      , current dText <@ ePostBuild
      ]

  ti <- textInput $
    def & textInputConfig_setValue .~
            eChanges

  let
    dValue = ti ^. textInput_value

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress
    eEscape   = ffilter (isKey Escape) eKeypress

    eAtEnter  = Text.strip <$> current dValue <@ eEnter
    eAtEscape =                current dText  <@ eEscape

    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , eAtEscape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter

  pure (eDone, eRemove)
```

<div id="examples-component-text-write"></div>

If we tie `textRead` and `textWrite` together using a `Workflow` we get a peak of how those two pieces work together:

<div id="examples-component-text"></div>

### When we don't want to create a `Dynamic t Text`

```haskell
textRead ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textRead iText = do
  (e, _) <- el' "div" $
    text iText
  pure . void $ domEvent Dblclick e
```

```haskell
textWrite ::
  MonadWidget t m =>
  Text ->
  m (Event t Text, Event t ())
textWrite iText = mdo
```

<div id="examples-component-text-di"></div>

## Putting the item together

TODO just slapping it all together

<div id="examples-component-todo-item"></div>

TODO doing things a bit more efficiently

<div id="examples-component-todo-item-d"></div>

## Adding the extra functionality
 
TODO bring mark all complete and clear complete into play

<div id="examples-component-todo-items"></div>

## Thinking more about what we want to model

TODO avoiding the need to model the text

TODO creating remove events inside the text component

TODO breaking the rules / having the complete item handle a bit more


<div id="examples-component-todo-item-di"></div>

<div id="examples-component-todo-items-i"></div>

## Playing along at home

There are exercises [here](../exercises/components/) that will put your understanding of the above content to the test.
These exercises build up incrementally as the series progresses, so it would probably best to start the exercises beginning at the start of the series.

## Next up

In the [next post](../collections/) we'll look at how to manage collections in `reflex`.
