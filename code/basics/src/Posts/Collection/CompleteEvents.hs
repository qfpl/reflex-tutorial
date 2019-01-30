{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Posts.Collection.CompleteEvents (
    todoList
  , ceMarkAllComplete
  , ceClearComplete
  ) where

import Control.Monad (void)

import Control.Monad.Reader (MonadReader, ask , runReaderT)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Posts.Collection.Common

data CompleteEvents t =
  CompleteEvents {
    _ceMarkAllComplete :: Event t Bool
  , _ceClearComplete :: Event t ()
  }

makeLenses ''CompleteEvents

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  B.button "Remove"

complete ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  Dynamic t Bool ->
  m (Event t (Bool -> Bool), Event t ())
complete dComplete = do
  initial <- sample . current $ dComplete
  CompleteEvents eMarkAllComplete eClearComplete <- ask

  let
    eChanges = leftmost [updated dComplete, eMarkAllComplete]

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges

  let
    eChange = const <$> cb ^. checkbox_change
    eRemove = gate (current dComplete) eClearComplete

  pure (eChange, eRemove)

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
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemRead dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  (eComplete, eRemoveComplete) <- complete dComplete
  eEditStart <- textRead dText
  eRemoveClick <- remove
  let
    eChange = over tiComplete <$> eComplete
    eRemove = leftmost [eRemoveClick, eRemoveComplete]

  pure ((eChange, eRemove), todoItemWrite dTodo <$ eEditStart)

todoItemWrite ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  Dynamic t TodoItem ->
  Workflow t m (Event t (TodoItem -> TodoItem), Event t ())
todoItemWrite dTodo = Workflow $ do
  dText <- holdUniqDyn $ view tiText <$> dTodo

  (eText, eRemove) <- textWrite dText

  let
    eChange = over tiText <$> eText

  pure ((eChange, eRemove), todoItemRead dTodo <$ eText)

todoItem ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
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
  [TodoItem] ->
  m (Dynamic t (Map Int TodoItem))
todoList i = mdo
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
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  dme <-
    flip runReaderT (CompleteEvents eMarkAllComplete eClearComplete) .
    el "ul" . list dModel $
      el "li" . todoItem
  let
    eChanges = switch . current . fmap (mergeMap . fmap fst) $ dme
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dme

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel
