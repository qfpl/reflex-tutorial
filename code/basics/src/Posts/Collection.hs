{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Posts.Collection (
    collectionPostExamples
  ) where

import Data.Semigroup hiding (Last(..))

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reset
import qualified Util.Bootstrap as B

addItemWidget ::
  MonadWidget t m =>
  m (Event t Text)
addItemWidget = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eClear)

  let
    dValue = ti ^. textInput_value

    eKeypress = ti ^. textInput_keypress
    isKey k   = (== k) . keyCodeLookup . fromIntegral
    eEnter    = ffilter (isKey Enter) eKeypress
    eEscape   = ffilter (isKey Escape) eKeypress

    eAtEnter  = Text.strip <$> current dValue <@ eEnter

    eDone = ffilter (not . Text.null) eAtEnter
    eClear = leftmost [void eDone, void eEscape]

  pure eDone

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
markAllComplete dAllComplete = el "div" $ do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  B.button "Remove"

complete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete
  let
    eChanges = updated dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges
  pure $ cb ^. checkbox_change

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

  eComplete <- complete dComplete
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
    , Map.union <$> eChanges
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
    , Map.union <$> eChanges
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

data ModelMulti k v =
  ModelMulti {
    _mmChanges :: Map k (Endo v)
  , _mmRemoves :: Set k
  }

makeLenses ''ModelMulti

instance Ord k => Semigroup (ModelMulti k v) where
  (<>) (ModelMulti m1 s1) (ModelMulti m2 s2) =
    ModelMulti (Map.unionWith (<>) m1 m2) (Set.union s1 s2)

instance Ord k => Monoid (ModelMulti k v) where
  mempty =
    ModelMulti mempty mempty
  mappend =
    (<>)

tellRemoveMM ::
  ( Reflex t
  , EventWriter t (ModelMulti k v) m
  , Ord k
  ) =>
  k ->
  Event t a ->
  m ()
tellRemoveMM k e =
  tellEvent $ (mempty & mmRemoves .~ Set.singleton k) <$ e

tellChangeMM ::
  ( Reflex t
  , EventWriter t (ModelMulti k v) m
  , Ord k
  ) =>
  k ->
  Event t v ->
  m ()
tellChangeMM k e =
  tellEvent $ (\v -> mempty & mmChanges .~ Map.singleton k (Endo $ const v)) <$> e

todoListEventWriter1 ::
  MonadWidget t m =>
  [TodoItem] ->
  m (Dynamic t (Map Int TodoItem))
todoListEventWriter1 i = mdo
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
    , Map.mergeWithKey (\_ f x -> Just (appEndo f x)) mempty id <$> eChanges
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    , fmap . set tiComplete <$> eMarkAllComplete
    , Map.filter (not . _tiComplete) <$ eClearComplete
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  (_, eMM) <- runEventWriterT . el "ul" . listWithKey dModel $ \k dv -> do
    (eChange, eRemove) <- el "li" . todoItem $ dv
    tellChangeMM k eChange
    tellRemoveMM k eRemove
    pure ()

  let
    eChanges = view mmChanges <$> eMM
    eRemoves = view mmRemoves <$> eMM

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel

data ModelSingle v =
  ModelSingle {
    _msChanges :: Endo v
  , _msRemoves :: Any
  }

makeLenses ''ModelSingle

instance Semigroup (ModelSingle v) where
  (<>) (ModelSingle f1 a1) (ModelSingle f2 a2) =
    ModelSingle (f1 <> f2) (a1 <> a2)

instance Monoid (ModelSingle v) where
  mempty =
    ModelSingle mempty mempty
  mappend =
    (<>)

tellRemoveMS ::
  ( Reflex t
  , EventWriter t (ModelSingle v) m
  ) =>
  Event t a ->
  m ()
tellRemoveMS e =
  tellEvent $ (mempty & msRemoves .~ Any True) <$ e

tellChangeMS ::
  ( Reflex t
  , EventWriter t (ModelSingle v) m
  ) =>
  Event t (v -> v) ->
  m ()
tellChangeMS e =
  tellEvent $ (\v -> mempty & msChanges .~ Endo v) <$> e

addKey ::
  ( Ord k
  ) =>
  k ->
  ModelSingle v ->
  ModelMulti k v
addKey k (ModelSingle e (Any b)) =
  ModelMulti (Map.singleton k e) $
    if b
    then Set.singleton k
    else Set.empty

adaptModel ::
  Lens' a b ->
  ModelSingle b ->
  ModelSingle a
adaptModel l (ModelSingle (Endo e) a) =
  ModelSingle (Endo (over l e)) a

withEventWriterT :: (Semigroup w, Semigroup w', Reflex t, MonadHold t m, MonadFix m)
                 => (w -> w')
                 -> EventWriterT t w m a
                 -> EventWriterT t w' m a
withEventWriterT f ew = do
  (r, e) <- lift $ do
    (r, e) <- runEventWriterT ew
    let e' = fmap f e
    return (r, e')
  tellEvent e
  pure r

todoItemRead2 ::
  ( MonadWidget t m
  ) =>
  Dynamic t TodoItem ->
  Workflow t (EventWriterT t (ModelSingle TodoItem) m) ()
todoItemRead2 dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  eComplete <- complete dComplete
  eEditStart <- textRead dText
  eRemove <- remove

  tellChangeMS $ set tiComplete <$> eComplete
  tellRemoveMS eRemove

  pure ((), todoItemWrite2 dTodo <$ eEditStart)

todoItemWrite2 ::
  ( MonadWidget t m
  ) =>
  Dynamic t TodoItem ->
  Workflow t (EventWriterT t (ModelSingle TodoItem) m) ()
todoItemWrite2 dTodo = Workflow $ do
  dText <- holdUniqDyn $ view tiText <$> dTodo

  (eText, eRemove) <- textWrite dText

  tellChangeMS $ set tiText <$> eText
  tellRemoveMS eRemove

  pure ((), todoItemRead2 dTodo <$ eText)

todoItem2 ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  EventWriterT t (ModelSingle TodoItem) m ()
todoItem2 =
  divClass "todo-item" .
    void . workflow . todoItemRead2

todoListEventWriter2 ::
  MonadWidget t m =>
  [TodoItem] ->
  m (Dynamic t (Map Int TodoItem))
todoListEventWriter2 i = mdo
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
    , Map.mergeWithKey (\_ f x -> Just (appEndo f x)) mempty id <$> eChanges
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    , fmap . set tiComplete <$> eMarkAllComplete
    , Map.filter (not . _tiComplete) <$ eClearComplete
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  (_, eMM) <- runEventWriterT . el "ul" . listWithKey dModel $ \k dv -> do
    el "li" . withEventWriterT (addKey k) . todoItem2 $ dv

  let
    eChanges = view mmChanges <$> eMM
    eRemoves = view mmRemoves <$> eMM

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel

remove3 ::
  MonadWidget t m =>
  EventWriterT t (ModelSingle ()) m ()
remove3 = do
  eRemove <- B.button "Remove"
  tellRemoveMS eRemove

complete3 ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t Bool ->
  EventWriterT t (ModelSingle Bool) m ()
complete3 eMarkAllComplete eClearComplete dComplete = do
  initial <- sample . current $ dComplete
  let
    eChanges = leftmost [eMarkAllComplete, updated dComplete]
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ eChanges

  tellChangeMS $ const <$> cb ^. checkbox_change
  tellRemoveMS $ gate (current (cb ^. checkbox_value)) eClearComplete

textRead3 ::
  MonadWidget t m =>
  Dynamic t Text ->
  EventWriterT t (ModelSingle Text) m (Event t ())
textRead3 dText = do
  (e, _) <- el' "div" $
    dynText dText
  pure . void $ domEvent Dblclick e

textWrite3 ::
  MonadWidget t m =>
  Dynamic t Text ->
  EventWriterT t (ModelSingle Text) m (Event t ())
textWrite3 dText = mdo
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
    eRemove = ffilter Text.null eAtEnter

  tellChangeMS $ const <$> eDone
  tellRemoveMS eRemove

  pure (void eDone)

todoItemRead3 ::
  ( MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  Workflow t (EventWriterT t (ModelSingle TodoItem) m) ()
todoItemRead3 eMarkAllComplete eClearComplete dTodo = Workflow $ do
  dComplete <- holdUniqDyn $ view tiComplete <$> dTodo
  dText     <- holdUniqDyn $ view tiText     <$> dTodo

  withEventWriterT (adaptModel tiComplete) $
    complete3 eMarkAllComplete eClearComplete dComplete

  eEditStart <- withEventWriterT (adaptModel tiText) $
    textRead3 dText

  withEventWriterT (adaptModel united)
    remove3

  pure ((), todoItemWrite3 eMarkAllComplete eClearComplete dTodo <$ eEditStart)

todoItemWrite3 ::
  ( MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  Workflow t (EventWriterT t (ModelSingle TodoItem) m) ()
todoItemWrite3 eMarkAllComplete eClearComplete dTodo = Workflow $ do
  dText <- holdUniqDyn $ view tiText <$> dTodo

  eEditStop <- withEventWriterT (adaptModel tiText) $ textWrite3 dText

  pure ((), todoItemRead3 eMarkAllComplete eClearComplete dTodo <$ eEditStop)

todoItem3 ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  EventWriterT t (ModelSingle TodoItem) m ()
todoItem3 eMarkAllComplete eClearComplete =
  divClass "todo-item" .
  void .
  workflow .
  todoItemRead3 eMarkAllComplete eClearComplete

todoListEventWriter3 ::
  MonadWidget t m =>
  [TodoItem] ->
  m (Dynamic t (Map Int TodoItem))
todoListEventWriter3 i = mdo
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
    , Map.mergeWithKey (\_ f x -> Just (appEndo f x)) mempty id <$> eChanges
    , (\ks -> Map.filterWithKey (\k _ -> k `notElem` ks)) <$> eRemoves
    ]

  eMarkAllComplete <- markAllComplete dAllComplete

  (_, eMM) <- runEventWriterT . el "ul" . listWithKey dModel $ \k dv -> do
    el "li" . withEventWriterT (addKey k) . todoItem3 eMarkAllComplete eClearComplete $ dv

  let
    eChanges = view mmChanges <$> eMM
    eRemoves = view mmRemoves <$> eMM

  let
    dComplete = fmap _tiComplete <$> dModel
    dAnyComplete = or <$> dComplete
    dAllComplete = and <$> dComplete

  eClearComplete <- clearComplete dAnyComplete

  pure dModel

simpleExample ::
  MonadWidget t m =>
  m ()
simpleExample = B.panel . reset $ do
  pure ()

todoListExample ::
  MonadWidget t m =>
  m ()
todoListExample = B.panel . reset $ do
  _ <- todoList
  pure ()

todoListInitExample ::
  MonadWidget t m =>
  m ()
todoListInitExample = B.panel . reset $ do
  _ <- todoListInit [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

todoListEventWriter1Example ::
  MonadWidget t m =>
  m ()
todoListEventWriter1Example = B.panel . reset $ do
  _ <- todoListEventWriter1 [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

todoListEventWriter2Example ::
  MonadWidget t m =>
  m ()
todoListEventWriter2Example = B.panel . reset $ do
  _ <- todoListEventWriter2 [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

todoListEventWriter3Example ::
  MonadWidget t m =>
  m ()
todoListEventWriter3Example = B.panel . reset $ do
  _ <- todoListEventWriter3 [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

collectionPostExamples ::
  MonadJSM m =>
  m ()
collectionPostExamples = do
  attachId_ "examples-collection-simple"
    simpleExample
  attachId_ "examples-collection-list"
    todoListExample
  attachId_ "examples-collection-list-initial"
    todoListInitExample
  attachId_ "examples-collection-eventwriter-1"
    todoListEventWriter1Example
  attachId_ "examples-collection-eventwriter-2"
    todoListEventWriter2Example
  attachId_ "examples-collection-eventwriter-3"
    todoListEventWriter3Example
  
