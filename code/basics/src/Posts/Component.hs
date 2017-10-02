{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Component (
    componentPostExamples
  ) where

import Control.Monad (void, join)

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
  eComplete <- divClass "todo-item" $
    completePostBuild dComplete

  dComplete <- holdDyn False eComplete
  el "div" $
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
  eComplete <- divClass "todo-item" $
    completeEndo dComplete

  dComplete <- foldDyn ($) False eComplete
  el "div" $
    display dComplete
  pure ()

completeI ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Bool ->
  m (Dynamic t Bool, Event t ())
completeI eMarkAllComplete eClearComplete iComplete = do

  cb <- checkbox iComplete $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    eChange = const <$> cb ^. checkbox_change
    dValue = cb ^. checkbox_value
    eRemove = void . ffilter id $ current dValue <@ eClearComplete

  pure (dValue, eRemove)

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

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
textWrite dText = mdo
  ePostBuild <- getPostBuild
  let
    eChanges = leftmost [
        eClear
      , current dText <@ ePostBuild
      ]

  ti <- textInput $
    def & textInputConfig_setValue .~
            eChanges

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , current dText <@ getKey ti Escape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter
    eClear = leftmost [
        "" <$ eDone
      , current dText <@ getKey ti Escape
      ]

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

  (eText, eRemove) <- textWrite dText

  let
    eChange = set tiText <$> eText

  pure ((eChange, eRemove), todoItemRead dTodo <$ eText)

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t ())
todoItem dTodo = divClass "todo-item" $ do
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
  el "div" $
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
  (eText, eRemove) <- textWrite dText
  let eChange =
        const <$> eText
  pure ((never, eChange, eRemove), todoItemDRead dComplete dText <$ eText)

todoItemD ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  m (Event t (Bool -> Bool), Event t (Text -> Text), Event t ())
todoItemD dComplete dText = divClass "todo-item" $ do
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
  el "div" $
    dynText dRemove

textReadDI ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textReadDI iText = do
  (e, _) <- el' "div" $
    text iText
  pure . void $ domEvent Dblclick e

textWriteDI ::
  MonadWidget t m =>
  Text ->
  m (Event t Text, Event t ())
textWriteDI iText = mdo
  ti <- textInput $
    def & textInputConfig_initialValue .~
            iText
        & textInputConfig_setValue .~
            eClear

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , iText <$ getKey ti Escape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter
    eClear = leftmost [
        "" <$ eDone
      , iText <$ getKey ti Escape
      ]

  pure (eDone, eRemove)

todoItemDIRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIRead dComplete iText = Workflow $ do
  eComplete <- completeEndo dComplete
  eEditStart <- textReadDI iText
  eRemove <- remove
  pure ((eComplete, eRemove), todoItemDIWrite dComplete iText <$ eEditStart)

todoItemDIWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  Workflow t m (Event t (Bool -> Bool), Event t ())
todoItemDIWrite dComplete iText = Workflow $ mdo
  (eText, eRemove) <- textWriteDI iText
  pure ((never, eRemove), todoItemDIRead dComplete <$> eText)

todoItemDI ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  m (Event t (Bool -> Bool), Event t ())
todoItemDI dComplete iText = divClass "todo-item" $ do
  dpe <- workflow $ todoItemDIRead dComplete iText
  let
    eComplete = switch . current . fmap fst $ dpe
    eRemove   = switch . current . fmap snd $ dpe
  pure (eComplete, eRemove)

todoItemDIExample ::
  MonadWidget t m =>
  m ()
todoItemDIExample = B.panel . reset $ mdo
  dComplete <- foldDyn ($) False eChangeComplete
  (eChangeComplete, eRemove) <- todoItemDI dComplete "Test me"

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  el "div" $
    dynText dRemove

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

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change

todoItemIRead ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  TodoItem ->
  Workflow t m (Dynamic t Bool, Event t ())
todoItemIRead eMarkAllComplete eClearComplete iTodo = Workflow $ do
  (dComplete, eRemoveClear) <- completeI eMarkAllComplete eClearComplete (view tiComplete iTodo)
  eEditStart <- textReadDI (view tiText iTodo)
  eRemoveClick <- remove
  let
    eTodo = (\c -> iTodo & tiComplete .~ c) <$> current dComplete <@ eEditStart
    eRemove = leftmost [eRemoveClear, eRemoveClick]
  pure ((dComplete, eRemove), todoItemIWrite eMarkAllComplete eClearComplete <$> eTodo)

todoItemIWrite ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  TodoItem ->
  Workflow t m (Dynamic t Bool, Event t ())
todoItemIWrite eMarkAllComplete eClearComplete iTodo = Workflow $ mdo
  (eText, eRemove) <- textWriteDI (view tiText iTodo)
  let
    dComplete = pure (view tiComplete iTodo)
    eTodo = (\t -> iTodo & tiText .~ t) <$> eText
  pure ((dComplete, eRemove), todoItemIRead eMarkAllComplete eClearComplete <$> eTodo)

todoItemI ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  TodoItem ->
  m (Dynamic t Bool, Event t ())
todoItemI eMarkAllComplete eClearComplete iTodo = divClass "todo-item" $ do
  dpe <- workflow $ todoItemIRead eMarkAllComplete eClearComplete iTodo
  let
    dComplete = join . fmap fst $ dpe
    eRemove   = switch . current . fmap snd $ dpe
  pure (dComplete, eRemove)

todoItemIExample ::
  MonadWidget t m =>
  m ()
todoItemIExample = B.panel . reset $ mdo
  (dCompleteA, eRemoveA) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem False "A"
  (dCompleteB, eRemoveB) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem True "B"
  (dCompleteC, eRemoveC) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem False "C"

  let
    dComplete = (\x y z -> [x, y, z]) <$> dCompleteA <*> dCompleteB <*> dCompleteC
    dAllComplete = and <$> dComplete
    dAnyComplete = or <$> dComplete
    eRemove = leftmost [eRemoveA, eRemoveB, eRemoveC]

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  el "div" $
    dynText dRemove

  eMarkAllComplete <- markAllComplete dAllComplete
  eClearComplete <- clearComplete dAnyComplete

  pure ()

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
  attachId_ "examples-component-todo-item-di"
    todoItemDIExample
