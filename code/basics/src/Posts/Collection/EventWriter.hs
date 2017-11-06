{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Posts.Collection.EventWriter (
    todoList
  ) where

import Control.Monad (void, join)

import Control.Monad.Reader (MonadReader, ask , runReaderT)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Posts.Collection.Common

data CompleteEvents t =
  CompleteEvents {
    _ceMarkAllComplete :: Event t Bool
  , _ceClearComplete :: Event t ()
  }

makeLenses ''CompleteEvents

tellRemove ::
  ( Reflex t
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  Event t a ->
  m ()
tellRemove k e =
  tellEvent $ Set.singleton k <$ e

remove ::
  ( MonadWidget t m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  m ()
remove k = do
  e <- B.button "Remove"
  tellRemove k e

complete ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  Bool ->
  m (Dynamic t Bool)
complete k iComplete = do
  CompleteEvents eMarkAllComplete eClearComplete <- ask

  cb <- checkbox iComplete $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dValue = cb ^. checkbox_value
    eRemove = gate (current dValue) eClearComplete

  tellRemove k eRemove

  pure dValue

textRead ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textRead iText = do
  (e, _) <- el' "div" $
    text iText
  pure . void $ domEvent Dblclick e

textWrite ::
  ( MonadWidget t m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  Text ->
  m (Event t Text)
textWrite k iText = mdo
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

  tellRemove k eRemove

  pure eDone

todoItemRead ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  TodoItem ->
  Workflow t m (Dynamic t Bool)
todoItemRead k iTodo = Workflow $ do
  dComplete <- complete k $ iTodo ^. tiComplete
  eEditStart <- textRead $ iTodo ^. tiText
  remove k

  pure (dComplete, todoItemWrite dComplete k iTodo <$ eEditStart)

todoItemWrite ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  Dynamic t Bool ->
  k ->
  TodoItem ->
  Workflow t m (Dynamic t Bool)
todoItemWrite dComplete k iTodo = Workflow $ do
  eText <- textWrite k $ iTodo ^. tiText

  let
    eTodo = (\t -> iTodo & tiText .~ t) <$> eText

  pure (dComplete, todoItemRead k <$> eTodo)

todoItem ::
  ( MonadWidget t m
  , MonadReader (CompleteEvents t) m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  k ->
  TodoItem ->
  m (Dynamic t Bool)
todoItem k iTodo = divClass "todo-item" $ do
  ddc <- workflow $ todoItemRead k iTodo
  pure . join $ ddc

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

  (dmdComplete, eRemoves) <-
    flip runReaderT (CompleteEvents eMarkAllComplete eClearComplete) .
    runEventWriterT .
    el "ul" . listWithKey dModel $ \k dv -> do
      i <- sample . current $ dv
      el "li" . todoItem k $ i

  let
    dComplete = joinDynThroughMap dmdComplete
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel
