{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Component (
    componentPostExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reset
import qualified Util.Bootstrap as B

{-
Options:
  D a -> E a
  D a -> E (a -> a)

  D (TodoItem B T)
  and break apart, if you need a complete model

  D (TodoItem (D B) (D T))
  if you don't need to break it apart

  look at how this interfaces with Workflow for read/edit

  look at the case where you don't need to pass the model elsewhere
  - in this case, we can emit a remove event from the textEdit and
    then we can model the text changes internally

  look at how this is effected by initial values

  need an aside on sample vs ePostBuild for initial values
  - safety vs rendering over a couple of frames

  look at generalizing this and using make classy and friends
-}

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

removeExample ::
  MonadWidget t m =>
  m ()
removeExample = B.panel . reset $ do
  eRemove <- remove
  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  dynText dRemove
  pure ()

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
  eComplete <- completeBasic False
  dComplete <- holdDyn False eComplete
  display dComplete
  pure ()

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
  dComplete <- holdDyn False eComplete
  eComplete <- completeSample dComplete
  display dComplete
  pure ()

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
  eComplete <- completePostBuild dComplete
  dComplete <- holdDyn False eComplete
  display dComplete
  pure ()

completeEndo ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t (Bool -> Bool))
completeEndo dComplete = do
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [updated dComplete, current dComplete <@ ePostBuild]
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges
  pure . fmap const $ cb ^. checkbox_change

completeEndoExample ::
  MonadWidget t m =>
  m ()
completeEndoExample = B.panel . reset $ mdo
  eComplete <- completeEndo dComplete
  dComplete <- foldDyn ($) False eComplete
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
  eEdit <- textRead $ pure "TODO"
  dEdit <- holdDyn "" $ "Edit" <$ eEdit
  dynText dEdit
  pure ()

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t (Text -> Text), Event t (), Event t ())
textWrite dText = mdo
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        "" <$ eClear
      , current dText <@ ePostBuild
      ]

  ti <- textInput $
    def & textInputConfig_setValue .~
            eChanges

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = ffilter (not . Text.null) eAtEnter
    eRemove = () <$ ffilter Text.null eAtEnter
    eClear = leftmost [void eDone, getKey ti Escape]

  pure (const <$> eDone, eRemove, eClear)

textWriteExample ::
  MonadWidget t m =>
  m ()
textWriteExample = B.panel . reset $ mdo
  (eChange, eRemove, eEditStop) <- textWrite dText
  dText <- foldDyn ($) "Test me" eChange
  dynText dText

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  dynText dRemove

  dEditStop <- holdDyn "" $ "Stop editing" <$ eEditStop
  dynText dEditStop

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText     :: Text
  }

makeLenses ''TodoItem

todoItemRead ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemRead dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  eComplete <- completeEndo dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  let
    -- for Event t Bool
    -- eChange = set tiComplete <$> eComplete
    -- for Event t (Bool -> Bool)
    eChange = over tiComplete <$> eComplete

  pure ((eChange, eRemove), todoItemWrite dTodo <$ eEditStart)

todoItemWrite ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemWrite dTodo = Workflow $ do
  dText <- holdUniqDyn $ view tiText <$> dTodo

  (eText, eRemove, eEditStop) <- textWrite dText

  let
    eChange = over tiText <$> eText

  pure ((eChange, eRemove), todoItemRead dTodo <$ eEditStop)

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t ())
todoItem dTodo = do
  dpe <- workflow $ todoItemRead dTodo
  let
    eChange = switch . current . fmap fst $ dpe
    eRemove = switch . current . fmap snd $ dpe
  pure (eChange, eRemove)

todoItemExample ::
  MonadWidget t m =>
  m ()
todoItemExample = B.panel . reset $ mdo
  dTodo <- foldDyn ($) (TodoItem False "Test me") eChange
  (eChange, eRemove) <- todoItem dTodo

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  dynText dRemove

todoItemDRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemDRead dComplete dText = Workflow $ do
  eComplete <- completeEndo dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  pure ((eComplete, never, eRemove), todoItemDWrite dComplete dText <$ eEditStart)

todoItemDWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemDWrite dComplete dText = Workflow $ do
  (eText, eRemove, eEditStop) <- textWrite dText
  pure ((never, eText, eRemove), todoItemDRead dComplete dText <$ eEditStop)

todoItemD ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemD dComplete dText = do
  dte <- workflow $ todoItemDRead dComplete dText
  let
    eComplete = switch . current . fmap (\(x, _, _) -> x) $ dte
    eText     = switch . current . fmap (\(_, x, _) -> x) $ dte
    eRemove   = switch . current . fmap (\(_, _, x) -> x) $ dte
  pure (eComplete, eText, eRemove)

todoItemDExample ::
  MonadWidget t m =>
  m ()
todoItemDExample = B.panel . reset $ mdo
  dComplete <- foldDyn ($) False eChangeComplete
  dText <- foldDyn ($) "Test me" eChangeText
  (eChangeComplete, eChangeText, eRemove) <- todoItemD dComplete dText

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  dynText dRemove

todoItemDIRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIRead dComplete dText = Workflow $ do
  eComplete <- completeEndo dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  pure ((eComplete, eRemove), todoItemDIWrite dComplete dText <$ eEditStart)

todoItemDIWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIWrite dComplete dText = Workflow $ do
  initial <- sample . current $ dText
  (eText, eRemove, eEditStop) <- textWrite (pure initial)
  dNewText <- foldDyn ($) initial eText
  pure ((never, eRemove), todoItemDIWrite dComplete dNewText <$ eEditStop)

todoItemDI ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  m (Event t (Bool -> Bool), Event t ())
todoItemDI dComplete dText = do
  dpe <- workflow $ todoItemDIRead dComplete dText
  let
    eComplete = switch . current . fmap fst $ dpe
    eRemove   = switch . current . fmap snd $ dpe
  pure (eComplete, eRemove)

todoItemDIExample ::
  MonadWidget t m =>
  m ()
todoItemDIExample = mdo
  dComplete <- foldDyn ($) False eChangeComplete
  let dText = pure "Test me"
  (eChangeComplete, eRemove) <- todoItemDI dComplete dText

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  dynText dRemove

componentPostExamples ::
  MonadJSM m =>
  m ()
componentPostExamples = do
  attachId_ "examples-component-remove"
    removeExample
  attachId_ "examples-component-complete-basic"
    completeBasicExample
  attachId_ "examples-component-complete-sample"
    completeSampleExample
  attachId_ "examples-component-complete-postbuild"
    completePostBuildExample
  attachId_ "examples-component-complete-endo"
    completeEndoExample
  attachId_ "examples-component-text-read"
    textReadExample
  attachId_ "examples-component-text-write"
    textWriteExample
  attachId_ "examples-component-todo-item"
    todoItemExample
  attachId_ "examples-component-todo-item-d"
    todoItemDExample
