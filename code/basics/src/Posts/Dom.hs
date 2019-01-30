{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Posts.Dom (
    domPostExamples
  , todoItemConfig_initialValue
  , todoItem_dComplete
  , todoItem_eRemove
  ) where

import Control.Monad (void)

import Control.Lens
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reset
import qualified Util.Bootstrap as B

todoExample ::
  MonadWidget t m =>
  m ()
todoExample = B.panel $
  el "div" $
    text "TODO"

todoItem0 ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
todoItem0 placeholder =
  el "div" $ do
    el "div" $
      text placeholder
    button "Remove"

todoItemExample1 ::
  MonadWidget t m =>
  m ()
todoItemExample1 = do
  _ <- todoItem0 "TODO"
  pure ()

todoItem1 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem1 dText =
  el "div" $ do
    el "div" $ dynText dText
    button "Remove"

todoItem2 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem2 dText = el "div" $ mdo
  eRemove <- el "div" $
    todoItem1 $ dText <> dLabel

  dLabel <- holdDyn "" $
    " (Removed)" <$ eRemove

  pure eRemove

todoItemExample2 ::
  MonadWidget t m =>
  m ()
todoItemExample2 = reset $ do
  _ <- todoItem2 $ pure "TODO"
  pure ()

todoItem3 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem3 dText =
  elClass "div" "todo-item" $ mdo

    elDynClass "div" dClass $
      dynText dText

    eRemove <- button "Remove"

    dClass <- holdDyn "" $
      "removed" <$ eRemove

    pure eRemove

todoItemExample3 ::
  MonadWidget t m =>
  m ()
todoItemExample3 = reset $ do
  _ <- todoItem3 $ pure "TODO"
  pure ()

todoItem4 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem4 dText =
  elClass "div" "todo-item" $ mdo

    elDynAttr "div" dAttr $
      dynText dText

    eRemove <- button "Remove"

    dAttr <- foldDyn (<>) mempty $
      "hidden" =: "" <$ eRemove

    pure eRemove

todoItemExample4 ::
  MonadWidget t m =>
  m ()
todoItemExample4 = reset $ do
  _ <- todoItem4 $ pure "TODO"
  pure ()

todoItem5 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem5 dText =
  elClass "div" "todo-item" $ mdo

    (e, _) <- elDynClass' "div" dClass $
      dynText dText
    let eDoubleClick = domEvent Dblclick e

    eRemove <- button "Remove"

    dClass <- holdDyn "" . leftmost $ [
                ""        <$ eDoubleClick
              , "removed" <$ eRemove
              ]

    pure eRemove

todoItemExample5 ::
  MonadWidget t m =>
  m ()
todoItemExample5 = reset $ do
  _ <- todoItem5 $ pure "TODO"
  pure ()

data TodoItemConfig =
  TodoItemConfig {
    _todoItemConfig_initialValue :: Text
  }

makeLenses ''TodoItemConfig

data TodoItem t =
  TodoItem {
    _todoItem_dComplete :: Dynamic t Bool
  , _todoItem_eRemove :: Event t ()
  }

makeLenses ''TodoItem

todoItem6 ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem6 (TodoItemConfig initialValue) =
  elClass "div" "todo-item" $ mdo

    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      text initialValue

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed" <$ eRemove

    pure $
      TodoItem dComplete eRemove

todoItemExample6 ::
  MonadWidget t m =>
  m ()
todoItemExample6 = reset $ do
  _ <- todoItem6 $ TodoItemConfig "TODO"
  pure ()

todoItem7 ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem7 (TodoItemConfig initialValue) =
  elClass "div" "todo-item" $ mdo

    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value
      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "
      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div" (dCompleteClass <> dRemoveClass) $
      text initialValue

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed " <$ eRemove

    pure $
      TodoItem dComplete eRemove

todoItemExample7 ::
  MonadWidget t m =>
  m ()
todoItemExample7 = reset $ do
  _ <- el "div" $ todoItem7 $ TodoItemConfig "TODO"
  pure ()

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

addItemWidget ::
  MonadWidget t m =>
  m (Event t Text)
addItemWidget = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eClear)

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = ffilter (not . Text.null) eAtEnter
    eClear = leftmost [void eDone, getKey ti Escape]

  pure eDone

todoItemExample8 ::
  MonadWidget t m =>
  m ()
todoItemExample8 = elClass "div" "add-item-wrapper" $ do

  eText <- el "div"
    addItemWidget

  dText <- holdDyn ""
    eText

  el "div" $
    dynText dText

  pure ()

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAnyComplete =
  let
    mkClass False = "hide"
    mkClass True  = ""
    dClass = mkClass <$> dAnyComplete
  in
    elDynClass "div" dClass $
      button "Clear complete"

clearCompleteExample ::
  MonadWidget t m =>
  m ()
clearCompleteExample = reset $ elClass "div" "add-item-wrapper" $ mdo

  let
    item = el "div" $ mdo
      cb <- checkbox False $
        def & checkboxConfig_attributes .~ dAttr
      let
        dComplete = cb ^. checkbox_value

      dCleared <- holdDyn False $
        True <$ gate (current dComplete) eClearComplete

      let
        attrFn False = mempty
        attrFn True  = "disabled" =: "true"
        dAttr = attrFn <$> dCleared

        textFn False = ""
        textFn True  = "Cleared"
        dText = textFn <$> dCleared

      dynText dText

      pure dComplete

  dComplete1 <- item
  dComplete2 <- item

  let
    dAnyComplete = (||) <$> dComplete1 <*> dComplete2

  eClearComplete <- el "div" $ clearComplete dAnyComplete

  pure ()

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change

markAllCompleteExample ::
  MonadWidget t m =>
  m ()
markAllCompleteExample = reset $ elClass "div" "add-item-wrapper" $ mdo
  cb1 <- el "div" . checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  cb2 <- el "div" . checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dComplete1 = cb1 ^. checkbox_value
    dComplete2 = cb2 ^. checkbox_value
    dAllComplete = (&&) <$> dComplete1 <*> dComplete2

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete

  pure ()

domPostExamples ::
  MonadJSM m =>
  m ()
domPostExamples = do
  attachId_ "examples-dom-todo" $
    B.panel todoExample
  attachId_ "examples-dom-todoitem-1" $
    B.panel todoItemExample1
  attachId_ "examples-dom-todoitem-2" $
    B.panel todoItemExample2
  attachId_ "examples-dom-todoitem-3" $
    B.panel todoItemExample3
  attachId_ "examples-dom-todoitem-4" $
    B.panel todoItemExample4
  attachId_ "examples-dom-todoitem-5" $
    B.panel todoItemExample5
  attachId_ "examples-dom-todoitem-6" $
    B.panel todoItemExample6
  attachId_ "examples-dom-todoitem-7" $
    B.panel todoItemExample7
  attachId_ "examples-dom-todoitem-8" $
    B.panel todoItemExample8
  attachId_ "examples-dom-clear-complete" $
    B.panel clearCompleteExample
  attachId_ "examples-dom-mark-all-complete" $
    B.panel markAllCompleteExample
