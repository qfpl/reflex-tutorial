{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Posts.Collection.NoModel (
    todoList
  ) where

import Control.Monad (void, join)

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
  Bool ->
  m (Dynamic t Bool, Event t ())
complete iComplete = do
  CompleteEvents eMarkAllComplete eClearComplete <- ask

  cb <- checkbox iComplete $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dValue = cb ^. checkbox_value
    eRemove = gate (current dValue) eClearComplete

  pure (dValue, eRemove)

textRead ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textRead iText = do
  (e, _) <- el' "div" $
    text iText
  pure . void $ domEvent Dblclick e

textWrite ::
  MonadWidget t m =>
  Text ->
  m (Event t Text, Event t ())
textWrite iText = mdo
  ti <- textInput $
    def & textInputConfig_initialValue .~
            iText
        & textInputConfig_setValue .~
            eDone

  let
    dValue = ti ^. textInput_value

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = void . ffilter (isKey Enter) $ eKeypress
    eEscape   = void . ffilter (isKey Escape) $ eKeypress

    eAtEnter  = Text.strip <$> current dValue <@ eEnter
    eAtEscape =                        iText  <$ eEscape

    eDone = leftmost [
        ffilter (not . Text.null) eAtEnter
      , eAtEscape
      ]
    eRemove = () <$ ffilter Text.null eAtEnter

  pure (eDone, eRemove)

todoItemRead ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  TodoItem ->
  Workflow t m (Dynamic t Bool, Event t ())
todoItemRead iTodo = Workflow $ do
  let
    iComplete = iTodo ^. tiComplete
    iText     = iTodo ^. tiText

  (dComplete, eRemoveComplete) <- complete iComplete
  eEditStart <- textRead iText
  eRemoveClick <- remove
  let
    eRemove = leftmost [eRemoveClick, eRemoveComplete]

  pure ((dComplete, eRemove), todoItemWrite dComplete iTodo <$ eEditStart)

todoItemWrite ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  Dynamic t Bool ->
  TodoItem ->
  Workflow t m (Dynamic t Bool, Event t ())
todoItemWrite dComplete iTodo = Workflow $ do
  let
    iText     = iTodo ^. tiText

  (eText, eRemove) <- textWrite iText

  let
    eTodo = (\t -> iTodo & tiText .~ t) <$> eText

  pure ((dComplete, eRemove), todoItemRead <$> eTodo)

todoItem ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  ) =>
  TodoItem ->
  m (Dynamic t Bool, Event t ())
todoItem iTodo = divClass "todo-item" $ do
  dpe <- workflow $ todoItemRead iTodo
  let
    dComplete = join . fmap fst $ dpe
    eRemove   = switch . current . fmap snd $ dpe
  pure (dComplete, eRemove)

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
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  dme <-
    flip runReaderT (CompleteEvents eMarkAllComplete eClearComplete) .
    el "ul" . list dModel $ \dv -> do
      i <- sample . current $ dv
      el "li" . todoItem $ i

  let
    dComplete = joinDynThroughMap . fmap (fmap fst) $ dme
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dme

  let
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel
