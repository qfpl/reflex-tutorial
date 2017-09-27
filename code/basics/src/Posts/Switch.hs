{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Posts.Switch (
    switchPostExamples
  ) where

import Control.Monad (void)
import Data.Monoid ((<>))

import Control.Lens

import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime)

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Grid
import Util.Reset
import qualified Util.Bootstrap as B
import Colour

type SwitchCountInput t m =
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t () ->
  m (Event t ())

leftInput1 :: SwitchCountInput t m
leftInput1 eAdd eSwitchL eSwitchR = do
  beAddL <- hold eAdd . leftmost $ [
              eAdd  <$ eSwitchL
            , never <$ eSwitchR
            ]
  pure (switch beAddL)

leftInput2 :: SwitchCountInput t m
leftInput2 eAdd eSwitchL eSwitchR =
  switchPromptly eAdd . leftmost $ [
    eAdd  <$ eSwitchL
  , never <$ eSwitchR
  ]

rightInput1 :: SwitchCountInput t m
rightInput1 eAdd eSwitchL eSwitchR = do
  beAddR <- hold never . leftmost $ [
              eAdd  <$ eSwitchR
            , never <$ eSwitchL
            ]
  pure (switch beAddR)

rightInput2 :: SwitchCountInput t m
rightInput2 eAdd eSwitchL eSwitchR =
  switchPromptly never . leftmost $ [
    eAdd  <$ eSwitchR
  , never <$ eSwitchL
  ]

countBlock ::
  MonadWidget t m =>
  Dynamic t Text ->
  Event t () ->
  m (Event t ())
countBlock dLabel eAdd =
  divClass "col-md-6" $ do
    eSwitch <- button "Select"

    divClass "center-block" $
      dynText dLabel

    dCount <- foldDyn ($) 0 $
              (+ 1) <$ eAdd

    divClass "center-block" $
      display dCount

    pure eSwitch

mkCountNetwork ::
  ( Reflex t
  , MonadHold t m
  ) =>
  SwitchCountInput t m ->
  SwitchCountInput t m ->
  Event t () ->
  Event t () ->
  Event t () ->
  m (Event t (), Event t ())
mkCountNetwork mkLeft mkRight eAdd eSwitchL eSwitchR = do
  eAddL <- mkLeft  eAdd eSwitchL eSwitchR
  eAddR <- mkRight eAdd eSwitchL eSwitchR
  pure (eAddL, eAddR)

switchCount ::
  MonadWidget t m =>
  SwitchCountInput t m ->
  SwitchCountInput t m ->
  m ()
switchCount mkLeft mkRight = B.panel . reset . divClass "container" $ mdo
  let
    selected =
      "Selected"
    notSelected =
      "Not selected"

  divClass "row" $ mdo
    (eAddL, eAddR) <- mkCountNetwork mkLeft mkRight eAdd eSwitchL eSwitchR

    dLabelL <- holdDyn selected .
               leftmost $ [
                  selected    <$ eSwitchL
                , notSelected <$ eSwitchR
                ]

    dLabelR <- holdDyn notSelected .
               leftmost $ [
                  selected    <$ eSwitchR
                , notSelected <$ eSwitchL
                ]

    eSwitchL <- countBlock dLabelL eAddL
    eSwitchR <- countBlock dLabelR eAddR

    pure ()

  eAdd <- divClass "row" . divClass "col-md-offset-3 col-md-6" $ do
    B.button "Add"

  pure ()

switchColour1 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour1 eSwitch1 eSwitch2 eInput = do
  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (eOut1, eOut2)

switchColour2 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour2 eSwitch1 eSwitch2 eInput = do
  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (switch bOut1, switch bOut2)

switchColourExample ::
  MonadWidget t m =>
  (Event t () -> Event t () -> Event t Colour -> m (Event t Colour, Event t Colour)) ->
  m ()
switchColourExample guest = el "div" $ mdo
  eSwitch1 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut1 ]
    pure eSwitch

  eSwitch2 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut2 ]
    pure eSwitch

  eInput <- mkRedBlueInput

  (eOut1, eOut2) <- guest eSwitch1 eSwitch2 eInput

  dOut1 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut1 
    , Nothing <$ eOut2
    ]

  dOut2 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut2
    , Nothing <$ eOut1
    ]

  pure ()

textWidget ::
  MonadWidget t m =>
  m (Event t Text)
textWidget = do
  ti <- textInput def
  pure $ ti ^. textInput_input

buttonWidget ::
  MonadWidget t m =>
  m (Event t Text)
buttonWidget = do
  eClick <- B.button "OK"
  pure $ "OK" <$ eClick

tickWidget ::
  MonadWidget t m =>
  m (Event t Text)
tickWidget = do
  now <- liftIO getCurrentTime
  eTick <- tickLossy 1 now
  el "div" $ text "Ticking..."
  pure $ (Text.pack . show . _tickInfo_n) <$> eTick

demoText ::
  MonadWidget t m =>
  m ()
demoText = B.panel $ do
  eText <- textWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

demoButton ::
  MonadWidget t m =>
  m ()
demoButton = B.panel $ do
  eText <- buttonWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

demoTick ::
  MonadWidget t m =>
  m ()
demoTick = B.panel $ do
  eText <- tickWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

hideExample ::
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
hideExample w = B.panel . elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    B.button "Switch"

  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle

    mkHidden False = "hidden" =: ""
    mkHidden True  = mempty

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynAttr "div" dHide1 $
    textWidget

  eText2 <- elDynAttr "div" dHide2 $
    w

  let
    eText =
      leftmost [
          gate (current    dToggle) eText1
        , gate (current dNotToggle) eText2
        ]

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]
  el "div" $
    dynText dText

holdExample ::
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
holdExample w = B.panel . elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    B.button "Switch"

  dToggle <- toggle True eSwitch

  let
    eShow1  = ffilter id  . updated $ dToggle
    eShow2  = ffilter not . updated $ dToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget <$ eShow1
    , w          <$ eShow2
    ]

  let
    eText  = switch . current $ deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText

dynExample ::
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
dynExample w = B.panel . elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    B.button "Switch"

  dToggle <- toggle True eSwitch

  let
    eShow1  = ffilter id  . updated $ dToggle
    eShow2  = ffilter not . updated $ dToggle

  dWidget <- holdDyn textWidget . leftmost $ [
      textWidget <$ eShow1
    , w          <$ eShow2
    ]

  eeText <- dyn dWidget
  eText <- switchPromptly never eeText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText

workflowExample ::
  forall t m.
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
workflowExample w = B.panel . elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    B.button "Switch"

  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eSwitch)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- w
      pure (eText, wf1 <$ eSwitch)

  deText <- workflow wf1

  let
    eText  = switch . current $ deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText

  pure ()

workflowExample1 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample1 = B.panel . elClass "div" "widget-hold-wrapper" $ mdo
  eSwitch <- el "div" $
    B.button "Switch"

  let
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

  deText <- workflow wf1

  let
    eText  = switch . current $ deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText

  pure ()

workflowExample2 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample2 = B.panel . elClass "div" "widget-hold-wrapper" $ do

  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      eNext <- el "div" $ B.button "Next"
      let eOut = leftmost [eText, "" <$ eNext]
      pure (eOut, wf2 <$ eNext)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      eBack <- el "div" $ B.button "Back"
      eNext <- el "div" $ B.button "Next"
      let eOut = leftmost [eText, "" <$ eBack, "" <$ eNext]
      pure (eOut, leftmost [wf1 <$ eBack, wf3 <$ eNext])

    wf3 :: Workflow t m (Event t Text)
    wf3 = Workflow $ do
      eText <- tickWidget
      eBack <- el "div" $ B.button "Back"
      let eOut = leftmost [eText, "" <$ eBack]
      pure (eOut, wf2 <$ eBack)

  deText <- workflow wf1

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

data TodoItemConfig =
  TodoItemConfig {
    _todoItemConfig_initialComplete :: Bool
  , _todoItemConfig_initialText     :: Text
  }

makeLenses ''TodoItemConfig

data TodoItem t =
  TodoItem {
    _todoItem_dComplete :: Dynamic t Bool
  , _todoItem_dText     :: Dynamic t Text
  , _todoItem_eRemove   :: Event t ()
  }

makeLenses ''TodoItem

complete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
complete dComplete  = do
  initialValue <- sample . current $ dComplete
  cb <- checkbox initialValue def
  pure $ cb ^. checkbox_change

edit ::
  MonadWidget t m =>
  Dynamic t Text ->
  Dynamic t Text ->
  m (Event t ())
edit dText dClass = do
  (e, _) <- elDynClass' "span" dClass $
    dynText dText
  pure $ () <$ domEvent Dblclick e

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

data ItemChanges t =
  ItemChanges {
    _itemChanges_eComplete :: Event t Bool
  , _itemChanges_eText     :: Event t Text
  , _itemChanges_eRemove   :: Event t ()
  }

todoItemRead ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (ItemChanges t)
todoItemRead dComplete dText = Workflow $ mdo
  eComplete <- complete dComplete
  let
    mkCompleteClass False = ""
    mkCompleteClass True  = "completed "
    dCompleteClass = mkCompleteClass <$> dComplete

  eEditStart <- edit dText (dCompleteClass <> dRemoveClass)

  eRemove <- remove

  dRemoveClass <- holdDyn "" $
    "removed " <$ eRemove

  pure (ItemChanges eComplete never eRemove, todoItemEdit dComplete dText <$ eEditStart)

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

todoItemEdit ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Dynamic t Text ->
  Workflow t m (ItemChanges t)
todoItemEdit dComplete dText = Workflow $ mdo
  initial <- sample . current $ dText

  ti <- textInput $
    def & textInputConfig_initialValue .~
            initial

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = ffilter (not . Text.null) eAtEnter
    eRemove = () <$ ffilter Text.null eAtEnter
    eEditStop = leftmost [void eDone, getKey ti Escape]

  pure (ItemChanges never eDone eRemove, todoItemRead dComplete dText <$ eEditStop)

todoItem ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem (TodoItemConfig iComplete iText) =
  elClass "div" "todo-item" $ mdo

    let
      eComplete = switch . current . fmap _itemChanges_eComplete $ dChanges
      eText     = switch . current . fmap _itemChanges_eText $ dChanges
      eRemove   = switch . current . fmap _itemChanges_eRemove $ dChanges

    dComplete <- holdDyn iComplete eComplete
    dText     <- holdDyn iText     eText

    dChanges <- workflow $ todoItemRead dComplete dText

    pure $ TodoItem dComplete dText eRemove

switchTodoItemExample ::
  MonadWidget t m =>
  m ()
switchTodoItemExample = B.panel . reset $ do
  _ <- el "div" $ todoItem $ TodoItemConfig False "TODO"
  pure ()

switchPostExamples ::
  MonadJSM m =>
  m ()
switchPostExamples = do
  attachId_ "examples-switch-count-1" $
    switchCount leftInput1 rightInput1
  attachId_ "examples-switch-count-2" $
    switchCount leftInput2 rightInput2

  attachId_ "examples-switch-colour-1" $
    switchColourExample switchColour1
  attachId_ "examples-switch-colour-2" $
    switchColourExample switchColour2

  attachId_ "examples-switch-demo-text" $
    demoText
  attachId_ "examples-switch-demo-button" $
    demoButton
  attachId_ "examples-switch-demo-tick" $
    demoTick

  attachId_ "examples-switch-hide-button" $
    hideExample buttonWidget
  attachId_ "examples-switch-hold-button" $
    holdExample buttonWidget
  attachId_ "examples-switch-dyn-button" $
    dynExample buttonWidget

  attachId_ "examples-switch-hide-tick" $
    hideExample tickWidget
  attachId_ "examples-switch-hold-tick" $
    holdExample tickWidget
  attachId_ "examples-switch-dyn-tick" $
    dynExample tickWidget

  attachId_ "examples-switch-workflow-button" $
    workflowExample buttonWidget
  attachId_ "examples-switch-workflow-tick" $
    workflowExample tickWidget

  attachId_ "examples-switch-workflow-1"
    workflowExample1
  attachId_ "examples-switch-workflow-2"
    workflowExample2

  attachId_ "examples-switch-todo"
    switchTodoItemExample
