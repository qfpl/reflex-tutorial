{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Component.Basic (
    todoItem
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import Util.Reset
import qualified Util.Bootstrap as B

import Posts.Component.Common

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  B.button "Remove"

removeExample ::
  MonadWidget t m =>
  m ()
removeExample = B.panel . reset $ do
  eRemove <- divClass "todo-item" $
    remove

  dRemove <- holdDyn "" $
    "Remove" <$ eRemove
  el "div" $
    dynText dRemove

completeBasic ::
  MonadWidget t m =>
  Bool ->
  m (Event t Bool)
completeBasic initial = do
  cb <- checkbox initial def
  pure $ cb ^. checkbox_change

completeBasicExample ::
  MonadWidget t m =>
  m ()
completeBasicExample = B.panel . reset $ do
  eComplete <- divClass "todo-item" $
    completeBasic False

  dComplete <- holdDyn False eComplete
  el "div" $
    display dComplete

completeSample ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
completeSample dComplete = do
  initial <- sample . current $ dComplete
  let
    eChanges = updated dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges
  pure $ cb ^. checkbox_change

completeSampleExample ::
  MonadWidget t m =>
  m ()
completeSampleExample = B.panel . reset $ mdo

  eComplete <- divClass "todo-item" $
    completeSample dComplete

  dComplete <- holdDyn False eComplete

  el "div" $
    display dComplete

completePostBuild ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
completePostBuild dComplete = do
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        updated dComplete
      , current dComplete <@ ePostBuild
      ]
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges
  pure $ cb ^. checkbox_change

completePostBuildExample ::
  MonadWidget t m =>
  m ()
completePostBuildExample = B.panel . reset $ mdo
  eComplete <- divClass "todo-item" $
    completePostBuild dComplete

  dComplete <- holdDyn False eComplete
  el "div" $
    display dComplete
  pure ()

textRead ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
textRead dText = do
  (e, _) <- el' "div" $
    dynText dText
  pure . void $ domEvent Dblclick e

textReadExample ::
  MonadWidget t m =>
  m ()
textReadExample = B.panel . reset $ do
  eEdit <- divClass "todo-item" $
    textRead $ pure "TODO"

  dEdit <- holdDyn "" $ "Edit" <$ eEdit
  el "div" $
    dynText dEdit
  pure ()

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

textWriteExample ::
  MonadWidget t m =>
  m ()
textWriteExample = B.panel . reset $ mdo
  (eChange, eRemove) <- divClass "todo-item" $
    textWrite dText

  dText <- holdDyn "Test me" . leftmost $ [
      eChange
    , "" <$ eRemove
    ]
  el "div" $
    dynText dText

  dRemove <- holdDyn "" . leftmost $ [
      "Remove" <$ eRemove
    , "" <$ eChange
    ]
  el "div" $
    dynText dRemove

textReadW ::
  MonadWidget t m =>
  Dynamic t Text ->
  Workflow t m (Event t Text, Event t ())
textReadW dText = Workflow $ do
  eEditStart <- textRead dText
  pure ((never, never), textWriteW dText <$ eEditStart)

textWriteW ::
  MonadWidget t m =>
  Dynamic t Text ->
  Workflow t m (Event t Text, Event t ())
textWriteW dText = Workflow $ do
  (eText, eRemove) <- textWrite dText
  let eEditStop = leftmost [() <$ eText, eRemove]
  pure ((eText, eRemove), textReadW dText <$ eEditStop)

textW ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
textW dText = do
  dpe <- workflow $ textReadW dText
  let
    eText   = switch . current . fmap fst $ dpe
    eRemove = switch . current . fmap snd $ dpe
  pure (eText, eRemove)

textExample ::
  MonadWidget t m =>
  m ()
textExample = B.panel . reset $ mdo
  (eText, eRemove) <- textW dText

  dText <- holdDyn "Test me" . leftmost $ [
      eText
    , "(Removed)" <$ eRemove
    ]

  pure ()

todoItemRead ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t TodoItem, Event t ())
todoItemRead dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  eComplete <- completePostBuild dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  let
    eChange = (\d c -> d & tiComplete .~ c) <$> current dTodo <@> eComplete

  pure ((eChange, eRemove), todoItemWrite dTodo <$ eEditStart)

todoItemWrite ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t TodoItem, Event t ())
todoItemWrite dTodo = Workflow $ do
  dText <- holdUniqDyn $ view tiText <$> dTodo

  (eText, eRemove) <- textWrite dText

  let
    eChange = (\d t -> d & tiText .~ t) <$> current dTodo <@> eText

  pure ((eChange, eRemove), todoItemRead dTodo <$ eText)

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t TodoItem, Event t ())
todoItem dTodo = divClass "todo-item" $ do
  dpe <- workflow $ todoItemRead dTodo
  let
    eChange = switch . current . fmap fst $ dpe
    eRemove = switch . current . fmap snd $ dpe
  pure (eChange, eRemove)
