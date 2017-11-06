{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Collection.Basic (
    todoList
  , todoListInit
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Posts.Collection.Common

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  B.button "Remove"

complete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t (Bool -> Bool))
complete dComplete = do
  initial <- sample . current $ dComplete
  let
    eChanges = updated dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges
  pure $ const <$> cb ^. checkbox_change

textRead ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
textRead dText = do
  (e, _) <- el' "div" $
    dynText dText
  pure . void $ domEvent Dblclick e

textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t (Text -> Text), Event t ())
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
    eEnter    = void . ffilter (isKey Enter) $ eKeypress
    eEscape   = void . ffilter (isKey Escape) $ eKeypress

    eAtEnter  = Text.strip <$> current dValue <@ eEnter
    eAtEscape =                current dText  <@ eEscape

    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , eAtEscape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter

  pure (const <$> eDone, eRemove)

todoItemRead ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemRead dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  eComplete <- complete dComplete
  eEditStart <- textRead dText
  eRemove <- remove
  let
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
    eChange = over tiText <$> eText

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

todoList ::
  MonadWidget t m =>
  m (Dynamic t (Map Int TodoItem))
todoList = mdo
  eAdd <- addItemWidget
  let
    eInsert = TodoItem False <$> eAdd

  dCount <- count eAdd

  dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eInsert
    , Map.mergeWithKey (\_ f x -> Just (f x)) mempty id <$> eChanges
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    , fmap . set tiComplete <$> eMarkAllComplete
    , Map.filter (not . _tiComplete) <$ eClearComplete
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  dme <- el "ul" . list dModel $ el "li" . todoItem
  let
    eChanges = switch . current . fmap (mergeMap . fmap fst) $ dme
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dme

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel

todoListInit ::
  MonadWidget t m =>
  [TodoItem] ->
  m (Dynamic t (Map Int TodoItem))
todoListInit i = mdo
  let
    initial = Map.fromList . zip [0..] $ i

  eAdd <- addItemWidget
  let
    eInsert = TodoItem False <$> eAdd

  dCount <- count eAdd
  let
    l = length i
    dKey = (+ l) <$> dCount

  dModel <- foldDyn ($) initial . mergeWith (.) $ [
      Map.insert <$> current dKey <@> eInsert
    , Map.mergeWithKey (\_ f x -> Just (f x)) mempty id <$> eChanges
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    , fmap . set tiComplete <$> eMarkAllComplete
    , Map.filter (not . _tiComplete) <$ eClearComplete
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  dme <- el "ul" . list dModel $ el "li" . todoItem
  let
    eChanges = switch . current . fmap (mergeMap . fmap fst) $ dme
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dme

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel
