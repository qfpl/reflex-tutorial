---
title: Working with the DOM
date: 2017-09-22
authors: dlaing
project: reflex
extra-css: /css/reflex/basics/grid-light.css, /css/reflex/basics/todo.css
extra-js: /js/reflex/basics/reflex-basics.min.js
---

[Previously](../dynamics/) we looked at `Dynamic`s, which combine the fundamental `Event` and `Behavior` types to allow us to do some things more efficiently.
Now we'll put them to use and actually create some DOM elements.

## The `DomBuilder` monad

When we want to create some DOM elements we use the `reflex-dom` library.

The main thing that it provides is an FRP aware builder monad for the DOM.

We can lay out elements using `el`:
```haskell
el :: DomBuilder t m 
   => Text 
   -> m a 
   -> m a
```
and can add text nodes with `text`:
```haskell
text :: DomBuilder t m 
     => Text 
     -> m ()
```
or `dynText`:
```haskell
dynText :: (PostBuild t m, DomBuilder t m) 
        => Dynamic t Text 
        -> m ()
```

As an aside: the `PostBuild` typeclass gives us access to an `Event` which fires when the element is added to the DOM tree, and is handy for setting up initial values and so on.

With the above pieces in hand, we can put together a simple `div` with some text in it using:
```haskell
el "div" $
  text "TODO"
```
or:
```haskell
el "div" $
  dynText $
    pure "TODO"
```

<div class="demo" id="examples-dom-todo"></div>

That's all well and good, but it's very static.

## Buttons

The simplest thing we can add to make things more interactive is a button.

We can add a button to our DOM tree with `button`:
```haskell
button :: DomBuilder t m 
       => Text 
       -> m (Event t ())
```

We can use this to create a very boring todo item:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t ())
todoItem dText =
  el "div" $ do
    el "div" $ dynText dText
    button "Remove"
```

<div class="demo" id="examples-dom-todoitem-1"></div>

This is following some common `reflex` advice about components: start with `Dynamic`s as inputs and `Event`s as outputs.
We'll come back to this later, and will see when to break those rules, but it's a very useful place to start.

It also introduces `MonadWidget`, which is a constraint synonym for a long list of typeclasses that are often used when created components that will be tanslated to parts of a DOM tree.

If we want to see something happen when that `Event` is fired, we can use it to modify the text we are displaying.

This gives us a marginally less boring todo item:
```haskell
example :: MonadWidget t m
        => Dynamic t Text
        -> m ()
example dTexet = el "div" $ mdo

  eRemove <- todoItem $ dText <> dLabel

  dLabel <- holdDyn "" $ 
    " (Removed)" <$ eRemove

  pure ()
```

<div class="demo" id="examples-dom-todoitem-2"></div>

## Various functions for creating elements

There are variants of the `el` function which allow for more customization.

### Adding classes

We can add a class to an element:
```haskell
elClass    :: DomBuilder t m 
           => Text 
           -> Text 
           -> m a 
           -> m a
```
or we can add class to an element that will change over time:
```haskell
elDynClass :: (DomBuilder t m, PostBuild t m) 
           => Text 
           -> Dynamic t Text 
           -> m a 
           -> m a
```

This lets us make our todo item a little prettier by adding a class to the item itself, and by using color to indicate when an item has been removed:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t ())
todoItem dText = elClass "div" "todo-item" $ mdo

  elDynClass "div" dRemoveClass $
    dynText dText

  eRemove <- button "Remove"

  dRemoveClass <- holdDyn "" $
    "removed" <$ eRemove

  pure eRemove
```

<div class="demo" id="examples-dom-todoitem-3"></div>

### Adding attributes

There are also functions which allow us to specify arbitrary attributes:
```haskell
elAttr    :: DomBuilder t m 
          => Text 
          -> Map Text Text 
          -> m a 
          -> m a

elDynAttr  :: (DomBuilder t m, PostBuild t m) 
           => Text 
           -> Dynamic t (Map Text Text) 
           -> m a 
           -> m a
```

We could use this to hide the text when an item is removed:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t ())
todoItem dText = elClass "div" "todo-item" $ mdo
  elDynAttr "div" dAttr $
    dynText dText

  eRemove <- button "Remove"

  dAttr <- foldDyn (<>) mempty $
    "hidden" =: "" <$ eRemove

  pure eRemove
```

The code above contains a use of the helper `=:`, which is the `Map.singleton` function in operator form.
It pops up in `reflex` code, so it's good to know about.

<div class="demo" id="examples-dom-todoitem-4"></div>

### Handling new events

All of the above functions for producing DOM elements have variants that expose the underlying element.

They all have a prime at the end of their names and return a pair.
For instance:
```haskell
el' :: DomBuilder t m 
    => Text 
    -> m a 
    -> m (Element EventResult (DomBuilderSpace m) t, a)
```

We can use these along with `domEvent`:
```haskell
class HasDomEvent t target eventName where
  type DomEventType target eventName :: *
  domEvent :: EventName eventName -> target -> Event t (DomEventType target eventName)
```
to create new `reflex ``Event`s from various DOM events.
It looks hideous, but it is fairly easy to use.

If we wanted a clickable link we could do something like:
```haskell
  (e, _) <- elAttr' "a" ("href" =: "#/clickey") $ text "Click me"
  domEvent Click e
```

To extend our previous example, we could clear the "removed" state of our item when the text is double clicked:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t ())
todoItem dText = elClass "div" "todo-item" $ mdo

  (e, _) <- elDynClass' "div" dClass $
    dynText dText

  let 
    eDoubleClick = domEvent Dblclick e

  eRemove <- button "Remove"

  dClass <- holdDyn "" . leftmost $ [
              ""        <$ eDoubleClick
            , "removed" <$ eRemove
            ]

  pure eRemove
```

<div class="demo" id="examples-dom-todoitem-5"></div>

We can use this to add support for any of the usual DOM events to our widgets.

## Checkboxes

There are more complicated inputs than buttons.

The simplest step up is a checkbox.
This gives us a little bit of insight into the design of components in `reflex-dom`.

We have a data type that contains the information we need to create the component:
```haskell
data CheckboxConfig t = 
  CheckboxConfig { 
      _checkboxConfig_setValue   :: Event   t Bool
    , _checkboxConfig_attributes :: Dynamic t (Map Text Text)
    }
```
which typically has a `Default` instance:
```haskell
instance Reflex t => Default (CheckboxConfig t) where ...
```
and we have a data type that contains the information we might want from the component:
```haskell
data Checkbox t = 
  Checkbox { 
     _checkbox_value  :: Dynamic t Bool
   , _checkbox_change :: Event   t Bool
   }
```

Both of these data types have lenses available.
We're only making basic usage of them for the time being, but they can be very useful.

Our use of lenses will be limited to setting up our configuration data types:
```haskell
  cb <- checkbox False $
    def & checkboxConfig_setValue   .~ eUpdateMe
        & checkboxConfig_attributes .~ pure ("disabled" =: "false")
```
and to accessing fields of the resulting data type:
```haskell
  eComplete = cb ^. checkbox_change
```

The two data types are linked together with a function that lays the checkbox out in the DOM tree - in this case it also has an argument for the initial state of the checkbox:
```haskell
checkbox :: MonadWidget t m
         => Bool 
         -> CheckboxConfig t 
         -> m (Checkbox t)
```

We're now ready to add a checkbox to our todo item:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t Bool, Event t ())
todoItem dText =
  elClass "div" "todo-item" $ mdo
    cb <- checkbox False def
    let
      eComplete = cb ^. checkbox_change

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    pure (eComplete, eRemove)
```


<div class="demo" id="examples-dom-todoitem-6"></div>

although we'd rather have some visual indicator that the checkbox is working correctly:
```haskell
todoItem :: MonadWidget t m 
         => Dynamic t Text 
         -> m (Event t Bool, Event t ())
todoItem dText =
  elClass "div" "todo-item" $ mdo
    cb <- checkbox False def
    let
      eComplete = cb ^. checkbox_change
      dComplete = cb ^. checkbox_value

      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "

      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div" (dCompleteClass <> dRemoveClass) $ 
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed " <$ eRemove

    pure (eComplete, eRemove)
```

<div class="demo" id="examples-dom-todoitem-7"></div>

We'll also use checkboxes for some other todo-list related functionality.

We can make a component we can use for clearing completed items, and have it only be visible when at least one todo item is completed:
```haskell
clearComplete :: MonadWidget t m 
              => Dynamic t Bool 
              -> m (Event t ())
clearComplete dAnyComplete =
  let
    mkClass False = "hide"
    mkClass True  = ""
    dClass = mkClass <$> dAnyComplete
  in
    elDynClass "div" dClass $
      button "Clear complete"
```

<div class="demo" id="examples-dom-clear-complete"></div>

We can also make a component that has a checkbox which causes all todo items to be marked as complete or incomplete, depending on the state of the checkbox.
We'll also make this binding bidirectional - if all of the items are marked as complete, the checkbox will become checked, and that ceases to be the case then the checkbox will become unchecked.

```haskell
markAllComplete :: MonadWidget t m 
                => Dynamic t Bool 
                -> m (Event t Bool)
markAllComplete dAllComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change
```

<div class="demo" id="examples-dom-mark-all-complete"></div>

## Text inputs

We're going to skip ahead, from the simple checkbox to the much more complex text input.

There are other inputs in `reflex-dom`, but once you can handle the checkbox and the text input you should be ready to use the other inputs without too much help.

The text input has a larger configuration data type:
```haskell
data TextInputConfig t = 
  TextInputConfig { 
      _textInputConfig_inputType    :: Text
    , _textInputConfig_initialValue :: Text
    , _textInputConfig_setValue     :: Event t Text
    , _textInputConfig_attributes   :: Dynamic t (Map Text Text)
    }
```
the usual `Default` instance:
```haskell
instance Reflex t => Default (TextInputConfig t) where ...
```
and a much larger data type for the information we might want from the component:
```haskell
data TextInput t = 
  TextInput { 
      _textInput_value          :: Dynamic t Text
    , _textInput_input          :: Event t Text
    , _textInput_keypress       :: Event t Word
    , _textInput_keydown        :: Event t Word
    , _textInput_keyup          :: Event t Word
    , _textInput_hasFocus       :: Dynamic t Bool
    , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t
    }
```

The initial value is part of the configuration data type now, so we can add these to the page with a slightly simpler function:
```haskell
textInput :: MonadWidget t m
          => TextInputConfig t 
          -> m (TextInput t)
```

We're going to use this to make an input we can use to add items to our todo list.
We want to fire an `Event` with the `Text` of the item that we want to add when there is text in the input and the user presses the enter key.
We also want to clear the input when that happens.

Let's put this together one piece at a time.

We'll start by putting a text input on the page:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def

  pure _
```
and then we'll set some placeholder text for it:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  pure _
```

We can get hold of the current value of the text input:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  let
    bValue    = current (ti ^. textInput_value)

  pure _
```
and by jumping through some type conversion hoops we can create an `Event` that fires when the user presses enter:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  let
    bValue    = current (ti ^. textInput_value)

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress

  pure _
```

We can use that to sample the value of the text input at the time that the user pressed enter:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  let
    bValue    = current (ti ^. textInput_value)

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress

    eAtEnter  = bValue <@ eEnter

  pure _
```
and we can filter out the times at which that happened while the text input was empty:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  let
    bValue    = current (ti ^. textInput_value)

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress

    eAtEnter  = bValue <@ eEnter
    eDone     = ffilter (not . Text.null) eAtEnter

  pure _
```

That gives us the `Event` that we wanted to return:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")

  let
    bValue    = current (ti ^. textInput_value)

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress

    eAtEnter  = bValue <@ eEnter
    eDone     = ffilter (not . Text.null) eAtEnter

  pure eDone
```
and thanks to the wonders of `RecursiveDo`, we can use that `Event` to clear the text input:
```haskell
addItem :: MonadWidget t m 
        => m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eDone)

  let
    bValue    = current (ti ^. textInput_value)

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress

    eAtEnter  = bValue <@ eEnter
    eDone     = ffilter (not . Text.null) eAtEnter

  pure eDone
```

It's the biggest piece of functionality that we have put together so far, but it does what is says on the tin:
<div class="demo" id="examples-dom-todoitem-8"></div>

## Next up

In the next post we'll look at some tools `reflex` provides for making modifications to the graph in response to `Event`s.
