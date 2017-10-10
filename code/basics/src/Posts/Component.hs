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
            eDone

  let
    bValue = current $ ti ^. textInput_value

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress
    eEscape   = ffilter (isKey Escape) eKeypress

    eAtEnter = Text.strip <$> bValue <@ eEnter
    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , iText <$ eEscape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter

  pure (eDone, eRemove)

textWReadDI ::
  MonadWidget t m =>
  Text ->
  Workflow t m (Event t ())
textWReadDI iText = Workflow $ do
  eEditStart <- textReadDI iText
  pure (never, textWWriteDI iText <$ eEditStart)

textWWriteDI ::
  MonadWidget t m =>
  Text ->
  Workflow t m (Event t ())
textWWriteDI iText = Workflow $ do
  (eText, eRemove) <- textWriteDI iText
  let eEditStop = leftmost [eText, "(Removed)" <$ eRemove]
  pure (eRemove, textWReadDI <$> eEditStop)

textDI ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textDI iText = do
  deRemove <- workflow $ textWReadDI iText
  pure . switch . current $ deRemove

textExampleDI ::
  MonadWidget t m =>
  m ()
textExampleDI = B.panel . reset $ mdo
  _ <- textDI "Test me"
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

todoItemExample ::
  MonadWidget t m =>
  m ()
todoItemExample = B.panel . reset $ mdo
  dTodo <- holdDyn (TodoItem False "Test me") eChange
  (eChange, eRemove) <- todoItem dTodo

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  el "div" $
    dynText dRemove

todoItemDRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t Bool, Event t Text, Event t ())
todoItemDRead dComplete dText = Workflow $ do
  eComplete <- completePostBuild dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  pure ((eComplete, never, eRemove), todoItemDWrite dComplete dText <$ eEditStart)

todoItemDWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (Event t Bool, Event t Text, Event t ())
todoItemDWrite dComplete dText = Workflow $ do
  (eText, eRemove) <- textWrite dText
  pure ((never, eText, eRemove), todoItemDRead dComplete dText <$ eText)

todoItemD ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  m (Event t Bool, Event t Text, Event t ())
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
  dComplete <- holdDyn False eComplete
  dText <- holdDyn "Test me" eText

  (eComplete, eText, eRemove) <- todoItemD dComplete dText

  dRemove <- holdDyn "" $ "Remove" <$ eRemove
  el "div" $
    dynText dRemove

todoItemDIRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  Workflow t m (Event t Bool, Event t ())
todoItemDIRead dComplete iText = Workflow $ do
  eComplete <- completePostBuild dComplete
  eEditStart <- textReadDI iText
  eRemove <- remove
  pure ((eComplete, eRemove), todoItemDIWrite dComplete iText <$ eEditStart)

todoItemDIWrite ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  Workflow t m (Event t Bool, Event t ())
todoItemDIWrite dComplete iText = Workflow $ mdo
  (eText, eRemove) <- textWriteDI iText
  pure ((never, eRemove), todoItemDIRead dComplete <$> eText)

todoItemDI ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Text ->
  m (Event t Bool, Event t ())
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
  dComplete <- holdDyn False eComplete
  (eComplete, eRemove) <- todoItemDI dComplete "Test me"

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

todoItemsExample ::
  MonadWidget t m =>
  m ()
todoItemsExample = B.panel . reset $ mdo
  dTodoA <- foldDyn ($) (TodoItem False "A") . leftmost $ [
      const <$> eChangeA
    , set tiComplete <$> eMarkAllComplete
    ]
  (eChangeA, eRemoveA) <- todoItem dTodoA

  let
    dCompleteA = view tiComplete <$> dTodoA
    eRemoveA' = leftmost [
        eRemoveA
      , void . ffilter id $ current dCompleteA <@ eClearComplete
      ]

  dRemoveA <- holdDyn "" $ "Remove" <$ eRemoveA'
  el "div" $
    dynText dRemoveA

  dTodoB <- foldDyn ($) (TodoItem True "B") . leftmost $ [
      const <$> eChangeB
    , set tiComplete <$> eMarkAllComplete
    ]
  (eChangeB, eRemoveB) <- todoItem dTodoB

  let
    dCompleteB = view tiComplete <$> dTodoB
    eRemoveB' = leftmost [
        eRemoveB
      , void . ffilter id $ current dCompleteB <@ eClearComplete
      ]

  dRemoveB <- holdDyn "" $ "Remove" <$ eRemoveB'
  el "div" $
    dynText dRemoveB

  dTodoC <- foldDyn ($) (TodoItem False "C") . leftmost $ [
      const <$> eChangeC
    , set tiComplete <$> eMarkAllComplete
    ]
  (eChangeC, eRemoveC) <- todoItem dTodoC

  let
    dCompleteC = view tiComplete <$> dTodoC
    eRemoveC' = leftmost [
        eRemoveC
      , void . ffilter id $ current dCompleteC <@ eClearComplete
      ]

  dRemoveC <- holdDyn "" $ "Remove" <$ eRemoveC'
  el "div" $
    dynText dRemoveC

  let
    dComplete = (\x y z -> [x, y, z]) <$> dCompleteA <*> dCompleteB <*> dCompleteC
    dAllComplete = and <$> dComplete
    dAnyComplete = or <$> dComplete

  eMarkAllComplete <- markAllComplete dAllComplete
  eClearComplete <- clearComplete dAnyComplete

  pure ()

todoItemsIExample ::
  MonadWidget t m =>
  m ()
todoItemsIExample = B.panel . reset $ mdo
  (dCompleteA, eRemoveA) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem False "A"

  dRemoveA <- holdDyn "" $ "Remove" <$ eRemoveA
  el "div" $
    dynText dRemoveA

  (dCompleteB, eRemoveB) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem True "B"

  dRemoveB <- holdDyn "" $ "Remove" <$ eRemoveB
  el "div" $
    dynText dRemoveB

  (dCompleteC, eRemoveC) <-
    todoItemI eMarkAllComplete eClearComplete $
      TodoItem False "C"

  dRemoveC <- holdDyn "" $ "Remove" <$ eRemoveC
  el "div" $
    dynText dRemoveC

  let
    dComplete = (\x y z -> [x, y, z]) <$> dCompleteA <*> dCompleteB <*> dCompleteC
    dAllComplete = and <$> dComplete
    dAnyComplete = or <$> dComplete

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
  attachId_ "examples-component-text-read"
    textReadExample
  attachId_ "examples-component-text-write"
    textWriteExample
  attachId_ "examples-component-text"
    textExample
  attachId_ "examples-component-text-di"
    textExampleDI
  attachId_ "examples-component-todo-item"
    todoItemExample
  attachId_ "examples-component-todo-item-d"
    todoItemDExample
  attachId_ "examples-component-todo-item-di"
    todoItemDIExample
  attachId_ "examples-component-todo-items"
    todoItemsExample
  attachId_ "examples-component-todo-items-i"
    todoItemsIExample
