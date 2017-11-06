{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Posts.Collection (
    collectionPostExamples
  ) where

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reset
import qualified Util.Bootstrap as B

import Posts.Collection.Common
import qualified Posts.Collection.Basic as Basic
import qualified Posts.Collection.CompleteEvents as CompleteEvents
import qualified Posts.Collection.NoModel as NoModel
import qualified Posts.Collection.EventWriter as EventWriter

basicTodoListExample ::
  MonadWidget t m =>
  m ()
basicTodoListExample = B.panel . reset $ do
  _ <- Basic.todoList
  pure ()

basicInitTodoListExample ::
  MonadWidget t m =>
  m ()
basicInitTodoListExample = B.panel . reset $ do
  _ <- Basic.todoListInit [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

completeEventsTodoListExample ::
  MonadWidget t m =>
  m ()
completeEventsTodoListExample = B.panel . reset $ do
  _ <- CompleteEvents.todoList [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

noModelTodoListExample ::
  MonadWidget t m =>
  m ()
noModelTodoListExample = B.panel . reset $ do
  _ <- NoModel.todoList [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

eventWriterTodoListExample ::
  MonadWidget t m =>
  m ()
eventWriterTodoListExample = B.panel . reset $ do
  _ <- EventWriter.todoList [TodoItem False "A", TodoItem True "B", TodoItem False "C"]
  pure ()

collectionPostExamples ::
  MonadJSM m =>
  m ()
collectionPostExamples = do
  attachId_ "examples-collection-basic"
    basicTodoListExample
  attachId_ "examples-collection-basic-init"
    basicInitTodoListExample
  attachId_ "examples-collection-completeEvents"
    completeEventsTodoListExample
  attachId_ "examples-collection-noModel"
    noModelTodoListExample
  attachId_ "examples-collection-eventWriter"
    eventWriterTodoListExample
