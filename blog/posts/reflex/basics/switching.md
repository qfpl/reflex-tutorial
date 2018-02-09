--
title: Switching
date: 2017-09-29
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css, /css/reflex/basics/todo.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

[Previously](../dom/) we looked at how to use `reflex-dom` to put DOM elements on a page.
Now we're going to look at how to modify our FRP network in response to user input.

## What is switching?

Everything we've done so far has involved building up an FRP network that is static.
The graph of dependencies between `Event`s, `Behavior`s and `Dynamic`s is set up in a particular configuration and it stays in that configuration for the lifetime of the application.
The same is true of our DOM elements - once we have laid them out on the page, they are there forever.

Sometimes we will want to modify the FRP network or the DOM in response to `Event`s, and that is what the various switching functions do.

There are two kinds of switching functions available to us:

- functions that modify the FRP network in response to input
- functions that modify the DOM in response to input

We'll cover these one at a time.

## Higher-order FRP

When we want to modify an FRP network, we do it using higher-order FRP.

We've probably seen higher-order functions, which is where function which take functions as arguments, like:
```haskell
map   :: (a -> b) 
      -> [a] 
      -> [b]
```
or
```haskell
foldr :: (a -> b -> b) 
      -> b 
      -> [a] 
      -> b
```

Higher-order FRP involves an FRP type - one of `Event`, `Behavior` or `Dynamic` - which contains another FRP type.

Some examples turn up due to the fact that `Behavior` and `Dynamic` have `Monad` instances, giving rise to:
```haskell
join :: Reflex t 
     => Behavior t (Behavior t a) 
     -> Behavior t a
```
and:
```haskell
join :: Reflex t 
     => Dynamic  t (Dynamic  t a) 
     -> Dynamic  t a
```

There are other functions provided by `reflex` that enable higher-order FRP, including:
```haskell
switch     :: Reflex t
           => Behavior t (Event t    a)
           ->             Event t    a

switcher   :: (Reflex t, MonadHold t m) 
           =>             Behavior t a
           -> Event t    (Behavior t a)
           -> m          (Behavior t a)

switchHold :: (Reflex t, MonadHold t m) 
           =>             Event t    a
           -> Event t    (Event t    a)
           -> m          (Event t    a)
```


It can be handy to think of a [railroad switch](https://en.wikipedia.org/wiki/Railroad_switch) while you're getting use to these methods.

We'll focus on `switch` for a while to motivate higher-order FRP:
```haskell
switch        :: Reflex t 
              => Behavior t (Event t    a)
              ->             Event t    a
```

We can view a `Behavior t (Event t a)` as an `Event t a` which is varying over time.
There could be multiple sources of these `Event`s, and the `Behavior` is being used to track which `Event` should be used at any given moment in time.
Since `Behavior`s have values at all points in time, that means there is always an `Event` which is selected.
The `switch` function is giving us access to the `Event` that is selected by the `Behavior`.

This time, we're going to look at the example before we look at the code:

<div id="examples-switch-count-1"></div>

Have a click around until you're comfortable that you know what is going on.

We're going to zoom in on the function that creates the `Event` that is used as the input to the counter on the left-hand side.

It takes in an `Event` for the "Add" button and both "Select" buttons.

We're going to create a `Behavior t (Event t ())` so that we can use `switch` to get the `Event` that we want out of this:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  -- beAddL :: Behavior t (Event t ())
  beAddL <- _
  pure (switch beAddL)
```

We build up our `Behavior` using `hold`:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  -- beAddL :: Behavior t (Event t ())
  beAddL <- hold _initial _change
  pure (switch beAddL)
```
which you might recall has this type signature:
```haskell
hold :: MonadHold t m
     => a
     -> Event t a
     -> m (Behavior t a)
```

Since we're calling `switch` like we get an `Event t ()` out of it, this leads to `a ~ Event t ()` in the above, which translates to:
```haskell
hold :: MonadHold t m
     => Event t ()
     -> Event t (Event t ())
     -> m (Behavior t (Event t ()))
```
for this particular use of `hold`.

We'll make a note of that:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  -- beAddL   :: Behavior t (Event t ())
  -- _initial :: Event    t ()
  -- _change  :: Event    t (Event t ())
  beAddL <- hold _initial _event
  pure (switch beAddL)
```

The "Add" button starts off being routed to the left input, so we'll use that as our initial value for our `Behavior`:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  -- beAddL  :: Behavior t (Event t ())
  -- _change :: Event    t (Event t ())
  beAddL <- hold eAdd _change
  pure (switch beAddL)
```

The `Behavior` is going to be changed whenever either of the "Select" buttons is pressed.
We use `leftmost` and the `Event`s coming from the "Select" buttons to start building up the other input to `hold`.
This leaves us with two things to fill in, but we know their types:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  -- beAddL       :: Behavior t (Event t ())
  -- _changeLeft  :: Event    t ()
  -- _changeRight :: Event    t ()
  beAddL <- hold eAdd . leftmost $ [
              _changeLeft  <$ eSwitchL
            , _changeRight <$ eSwitchR
            ]
  pure (switch beAddL)
```

We also know that we want the input from the "Add" button to flow through to the output when the output on the left-hand side is selected, and
that we want no input to flow through to the output when the output on the right-hand side is selected.
We have those things at hand:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR = do
  beAddL <- hold eAdd . leftmost $ [
              eAdd  <$ eSwitchL
            , never <$ eSwitchR
            ]
  pure (switch beAddL)
```
and we are done.

Switching the "Add" clicks towards the output on the right-hand side is very similar:
```haskell
rightInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
rightInput eAdd eSwitchL eSwitchR = do
  -- beAddR :: Behavior t (Event t ())
  beAddR <- hold never . leftmost $ [
              eAdd  <$ eSwitchR
            , never <$ eSwitchL
            ]
  pure (switch beAddR)
```

We can do this a little more directly using `switchHold`:
```haskell
switchHold :: (Reflex t, MonadHold t m) 
           =>             Event t    a
           -> Event t    (Event t    a)
           -> m          (Event t    a)
```
The first argument is the initial `Event` to use as the output.
The second argument consists of an outer `Event` and an inner `Event`.
The outer `Event` fires to it indicate that the output should switch.
It switches to the value of the inner `Event` until such time that the outer `Event` fires again.

This results in:
```haskell
leftInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
leftInput eAdd eSwitchL eSwitchR =
  switchHold eAdd . leftmost $ [
    eAdd  <$ eSwitchL
  , never <$ eSwitchR
  ]
```
and
```haskell
rightInput :: (Reflex t, MonadHold t m) 
          => Event t () 
          -> Event t () 
          -> Event t () 
          -> m (Event t ())
rightInput eAdd eSwitchL eSwitchR =
  switchHold never . leftmost $ [
    eAdd  <$ eSwitchR
  , never <$ eSwitchL
  ]
```

The resulting widget still behaves the same way:
<div id="examples-switch-count-2"></div>

If you've been paying close attention, you might be wondering if we could have just used `gate` or `ffilter` to do something like this.
For this example, you certainly could.

The benefits of modifying the FRP network start to come into play once pieces of your network start to consume significant amounts of processing time or memory.
In those cases, you can use switching to only add those pieces to your FRP network when you need them and to remove them once you no longer need them.

If some piece of the network becomes entirely disconnected from the rest of the network then it is eligible for garbage collection, and so you'll be able to reclaim the memory it was using and rest easy knowing that it isn't hanging around and using up your processing time.

## Dynamic modifications to the DOM

Imagine that we have three widgets that all return an `Event t Text`.

We have a text widget which fires its `Event` when the user types:
<div id="examples-switch-demo-text"></div>
a button widget which fires its `Event` when the user clicks the buttton:
<div id="examples-switch-demo-button"></div>
and a tick widget with fires its `Event` every second and doesn't care about the user at all:
<div id="examples-switch-demo-tick"></div>

We'd like to switch between displaying these widgets and collect the `Event t Text` from whichever widget is being displayed.

### Faking it by hiding elements

Imagine that we are very keen on premature optimization, and we know that modifying the structure of the DOM is slow but modifying attributes on the DOM is fast.

We could solve this problem by hiding elements and gating their outputs based on whether or not they are displayed.

We start by putting a button on the page:
```haskell
hideWidget :: MonadWidget t m 
           => m ()
hideWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"
```
and we track how many times it has been toggled:
```haskell
  dToggle <- toggle True eSwitch
```

This is using a handy helper function from `reflex` in passing:
```haskell
toggle :: MonadHold t m 
       => Bool 
       -> Event a 
       -> m (Dynamic Bool)
```
to toggle a `Bool` when an `Event` fires.

We also set up a `Dynamic` with the negation of that `Bool`, because it will be handy in a moment:
```haskell
  let
    dNotToggle = not <$> dToggle
```

We write a helper function to toggle the "hidden" attribute based on a `Bool`:
```haskell
    mkHidden False = "hidden" =: ""
    mkHidden True  = mempty
```
which we use to create a pair of `Dynamic` attribute maps:
```haskell
    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle
```

Now we can wrap our widgets in divs that will be hidden or shown based on how many times the button has been pressed:
```haskell
  eText1 <- elDynAttr "div" dHide1
    textWidget

  eText2 <- elDynAttr "div" dHide2
    buttonWidget
```

We need to gate the outputs so that only `Event`s from the currently displayed widget flow through:
```haskell
  let
    eText =
      leftmost [
          gate (current    dToggle) eText1
        , gate (current dNotToggle) eText2
        ]
```

That output gets turned into a `Dynamic` so that we can display it, which we clear whenever the "Switch" button is pressed:
```haskell
  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]
```
after which we put it on the page:
```haskell
  el "div" $
    dynText dText
```

Here it is in one piece:
```haskell
hideWidget :: MonadWidget t m 
           => m ()
hideWidget = el "div" $ do
  -- Set up a button to switch between inputs
  eSwitch <- el "div" $
    button "Switch"

  -- Toggle a `Bool` when the button is pressed
  dToggle <- toggle True eSwitch

  let
    -- Track the opposite state as well
    dNotToggle = not <$> dToggle

    -- Hide elements based on a `Bool`
    mkHidden False = "hidden" =: ""
    mkHidden True  = mempty

    -- Build a Dynamic class based on how 
    -- many times "Switch" has been pressed
    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  -- Show the text widget for an even number of presses
  eText1 <- elDynAttr "div" dHide1
    textWidget

  -- Show the text widget for an odd number of presses
  eText2 <- elDynAttr "div" dHide2
    buttonWidget

  let
    -- Gate the outputs from the widget so that they don't step on each other
    eText =
      leftmost [
          gate (current    dToggle) eText1
        , gate (current dNotToggle) eText2
        ]

  -- Clear the output when switching occurs
  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  -- Display the output 
  el "div" $
    dynText dText
```

If we click around with this:
<div id="examples-switch-hide-button"></div>
we'll see that we're definitely not adding or removing elements from the DOM, because the state of the text input is being maintained across clicks of the "Switch" button.

This is even more pronounced if we use the timer widget in place of the button widget:
<div id="examples-switch-hide-tick"></div>

### Switching out elements

If want to have freshly laid out widgets every time we click the "Switch" button, we need some new functions.

The first of these is `widgetHold`:
```haskell
-- The constraint is actually `DomBuilder t m`, but these are the constraints that
-- a) are implied by `DomBuilder t m`, and
-- b) are used by `widgetHold`
widgetHold :: (Reflex t, Monad m, MonadAdjust t m) 
           =>               m a 
           ->    Event   t (m a) 
           -> m (Dynamic t    a)
```

The first argument is the initial widget to lay out on the page.
The second argument is an `Event` with the next widget to lay out on the page as its value.
The values that these widgets return are collected into a `Dynamic`.
We'll see why you would want this in a moment.

The `MonadAdjust` typeclass is present here so that we can replace pieces of the FRP network.

You might wonder how we managed without this for the switching functions at the beginning of this post.
If we think of an FRP network as a graph, the earlier switch function were moving edges around between nodes.
We could completely remove a piece of the graph - with help from the garbage collector - if nothing is connected to it and if we know that nothing will ever be connected to it again, but that is the only way we could effect the nodes via switching.
For all other cases where we want to add or remove nodes, we need the `MonadAdjust` typeclass to place and connect new nodes when certain `Event`s fire.

In the same way that we have `never` for when we need an `Event` which doesn't fire, we have `blank` for when we need a widget that doesn't display on the page.
Sometimes that is useful as an initial value for `widgetHold`, but not always.

With all of that out of the way, let us have a look at how we might use `widgetHold`.

We start with the same button and toggling `Dynamic` that we used for `hideWidget`:
```haskell
holdWidget :: MonadWidget t m 
           => m ()
holdWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch
```

To use `widgetHold` we're going to need `Event`s that trigger when we want to change widgets, so we set some up:
```haskell
  let
    eShow1  = ffilter id  . updated $ dToggle
    eShow2  = ffilter not . updated $ dToggle
```

Now that we have the pieces in place, we use `widgetHold` to put `textWidget` on the page, and to switch between the two widgets depending on how many times the "Switch" button has been pressed:
```haskell
  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]
```

This gives us a `Dynamic t (Event t Text)`, and we want an `Event t Text`.

There is a function with this signature:
```haskell
switchDyn :: Dynamic t (Event t a)
          -> Event t a

```

In our case, we use it to pull out an `Event t Text`:
```haskell
  let
    eText = switchDyn deText
```

At that point we have what we need to display the output on the page as before:
```haskell
  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText
```

All together it looks like:
```haskell
holdWidget :: MonadWidget t m 
           => m ()
holdWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    -- This will fire when `dToggle` changes to `True`
    eShow1  = ffilter id  . updated $ dToggle
    -- This will fire when `dToggle` changes to `False`
    eShow2  = ffilter not . updated $ dToggle

  -- Builds up a `Dynamic t (Event t Text)`
  -- Starts with `textWidget`
  deText <- widgetHold textWidget . leftmost $ [
      -- Switch to `textWidget` when `dToggle` change to `True`
      textWidget   <$ eShow1
      -- Switch to `buttonWidget` when `dToggle` change to `False`
    , buttonWidget <$ eShow2
    ]

  let
    -- Collapse the `Dynamic t (Event t Text)` to an `Event t Text`
    eText = switchDyn deText

  -- Clear the output when switching occurs
  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  -- Display the output
  el "div" $
    dynText dText
```

We can see that we are getting freshly laid out DOM elements every time we press "Switch" by playing with this:
<div id="examples-switch-hold-button"></div>
and with this:
<div id="examples-switch-hold-tick"></div>

These are small examples, but the idea gets more useful as you do more adventurous things.

If we don't know what we want to use as an initial value for `widgetHold`, we can use `dyn`:
```haskell
dyn        :: (DomBuilder t m, PostBuild t m) 
           =>    Dynamic t (m a) 
           -> m (Event   t    a)
```
although `widgetHold` is probably a better bet if you have a choice between the two.


To use it, we would start with something that should look very familiar:
```haskell
dynWidget :: MonadWidget t m 
           => m ()
dynWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    eShow1  = ffilter id  . updated $ dToggle
    eShow2  = ffilter not . updated $ dToggle
```

We'll build a `Dynamic` of widgets that return `Event t Text`:
```haskell
  dWidget <- holdDyn textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]
```
and we'll use `dyn` to collect the outptuts into an `Event t (Event t Text)`:
```haskell
  eeText <- dyn dWidget
```

We can collapse these to an `Event t Text` using `switchHold`, using `never` as the inital `Event`:
```haskell
  eText  <- switchHold never eeText
```
and then we proceed as normal:
```haskell
  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText
```

All in one place it looks like this:
```haskell
dynWidget :: MonadWidget t m 
           => m ()
dynWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    eShow1  = ffilter id  . updated $ dToggle
    eShow2  = ffilter not . updated $ dToggle

  -- Builds up a `Dynamic` of widgets that return `Event t Text`:
  dWidget <- holdDyn textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]

  -- Using `dyn` on this gives us an `Event t (Event t Text)`:
  eeText <- dyn dWidget
  -- and we can use `switchHold` to turn that into an `Event t Text`:
  eText  <- switchHold never eeText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText
```

### Using `Workflow`

We can use a handy piece of functionality to clean this up a little.
It might look scary at first glance, but we'll get used to it pretty quickly.

A `Workflow` is a `newtype` used to build a graph of widgets that the user will transition through.
It's the kind of thing you would reach for if you were building a "wizard" in a UI, but it is much more flexible than that.

The `newtype` wraps a widget that returns a pair, containing the result we are interested in and an `Event` which will fire with the next piece of the `Workflow` we want to visit:
```haskell
newtype Workflow t m a = Workflow { 
    unWorkflow :: m (a, Event t (Workflow t m a))
  }
```

Once we have that assembled, we can run it with the `workflow` function:
```haskell
workflow :: (DomBuilder t m, MonadFix m, MonadHold t m) 
         => Workflow t m a 
         -> m (Dynamic t a)
```
and it will give us a `Dynamic` that collects the changing result values as the user interacts with the workflow.

An example will help.

We set up a "Switch button":
```haskell
workflowWidget :: MonadWidget t m => m ()
workflowWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"
```
and then we start creating pieces of the workflow.

The first piece will lay out the `textWidget` on the page, and will transition to the second piece of the workflow when "Switch" is pressed:
```haskell
  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eSwitch)
```

The second piece will lay out the `tickWidget` on the page, and will transition to the first piece of the workflow when "Switch" is pressed:
```haskell
    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- tickWidget
      pure (eText, wf1 <$ eSwitch)
```

We don't have to worry about rigging up a toggle `Event` and keeping everything synchronized, we just set up the graph for the user to navigate.

We start the user on the first piece of the workflow:
```haskell
  deText <- workflow wf1
```
which gives us a `Dynamic t (Event t Text)`, and we know what to do with that:
```haskell
  let
    eText  = switchDyn deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText
```

All in one place it looks like:
```haskell
workflowWidget :: MonadWidget t m => m ()
workflowWidget = el "div" $ do
  eSwitch <- el "div" $
    button "Switch"

  let
    -- Setup a piece of a workflow that 
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      -- puts a `textWidget on the page`
      eText <- textWidget
      -- and moves to another piece of the workflow when "Switch" is pressed:
      pure (eText, wf2 <$ eSwitch)

    -- Setup a piece of a workflow that 
    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      -- puts a `tickWidget` on the page
      eText <- tickWidget
      -- and moves to another piece of the workflow when "Switch" is pressed:
      pure (eText, wf1 <$ eSwitch)

  -- Run the workflow and get a hold of the `Dynamic` that collects the results 
  -- of the journey through the workflow
  deText <- workflow wf1

  let
    eText  = switchDyn deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText
```

This gives us the same behavior as we had previously:

<div id="examples-switch-workflow-tick"></div>

We can use this to switch between our various widgets in a cycle:

```haskell
wf1 :: Workflow t m (Event t Text)
wf1 = Workflow $ do
  eText <- textWidget
  pure (eText, wf2 <$ eSwitch)

wf2 :: Workflow t m (Event t Text)
wf2 = Workflow $ do
  eText <- buttonWidget
  pure (eText, wf3 <$ eSwitch)

wf3 :: Workflow t m (Event t Text)
wf3 = Workflow $ do
  eText <- tickWidget
  pure (eText, wf1 <$ eSwitch)
```

<div id="examples-switch-workflow-1"></div>

or we can give each widget it's own "Next" and "Back" buttons to arrange them more like a traditional wizard:

```haskell
wf1 :: Workflow t m (Event t Text)
wf1 = Workflow $ do
  eText <- textWidget
  eNext <- el "div" $ button "Next"
  let eOut = leftmost [eText, "" <$ eNext]
  pure (eOut, wf2 <$ eNext)

wf2 :: Workflow t m (Event t Text)
wf2 = Workflow $ do
  eText <- buttonWidget
  eBack <- el "div" $ button "Back"
  eNext <- el "div" $ button "Next"
  let eOut = leftmost [eText, "" <$ eBack, "" <$ eNext]
  pure (eOut, leftmost [wf1 <$ eBack, wf3 <$ eNext])

wf3 :: Workflow t m (Event t Text)
wf3 = Workflow $ do
  eText <- tickWidget
  eBack <- el "div" $ button "Back"
  let eOut = leftmost [eText, "" <$ eBack]
  pure (eOut, wf2 <$ eBack)
```

<div id="examples-switch-workflow-2"></div>

## Playing along at home

If you want to test out your understanding of how switching works, there are exercises coming soon.
<!--If you want to test out your understanding of how switching works, there are exercises [here](../exercises/switching/) that might help.-->
These exercises build up incrementally as the series progresses, so it would probably best to start the exercises beginning at the start of the series.

## Next up

In the next post we'll look at how we break things up into components in `reflex`, and the various design tradeoffs that are involved with that.
<!--In the [next post](../components/) we'll look at how we break things up into components in `reflex`.-->
