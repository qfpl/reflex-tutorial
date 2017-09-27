{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

completeBasic ::
  MonadWidget t m =>
  Bool ->
  m (Event t Bool)
completeBasic initial = do
  cb <- checkbox initial def
  pure $ cb ^. checkbox_change

completeSample ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
completeSample dComplete = do
  initial <- sample . current $ dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete
  pure $ cb ^. checkbox_change

completePostBuild ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
completePostBuild dComplete = do
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [updated dComplete, current dComplete <@ ePostBuild]
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eChanges
  pure $ cb ^. checkbox_change

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

completeExample ::
  MonadWidget t m =>
  m ()
completeExample =
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
textReadExample =
  pure ()

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

removeExample ::
  MonadWidget t m =>
  m ()
removeExample =
  pure ()

textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
textWrite dText =
  _

textWriteExample ::
  MonadWidget t m =>
  m ()
textWriteExample =
  pure ()

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

  pure (_, todoItemWrite dTodo <$ _)

todoItemWrite ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemWrite dTodo = Workflow $ do

  pure (_, todoItemRead dTodo <$ _)

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
todoItemExample =
  pure ()

todoItemDRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemDRead dComplete dText = Workflow $ do

  pure (_, todoItemDWrite dComplete dText <$ _)

todoItemDWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemDWrite dComplete dText = Workflow $ do

  pure (_, todoItemDRead dComplete dText <$ _)

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
todoItemDExample =
  pure ()

todoItemDIRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIRead dComplete dText = Workflow $ do

  pure (_, todoItemDIWrite dComplete dText <$ _)

todoItemDIWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIWrite dComplete dText = Workflow $ do

  pure (_, todoItemDIRead dComplete dText <$ _)

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
todoItemDIExample =
  pure ()

componentPostExamples ::
  MonadJSM m =>
  m ()
componentPostExamples = do
  attachId_ "examples-component-complete" $
    completeExample
  attachId_ "examples-component-text-read" $
    textReadExample
  attachId_ "examples-component-remove" $
    removeExample
  attachId_ "examples-component-text-write" $
    textWriteExample
  attachId_ "examples-component-todo-item" $
    todoItemExample
