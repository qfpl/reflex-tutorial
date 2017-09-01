{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Util.Attach (
    attachId
  , attachId_
  ) where

import qualified Data.Text as T

import Reflex.Dom.Core
import GHCJS.DOM
import GHCJS.DOM.NonElementParentNode
import Language.Javascript.JSaddle.Monad (MonadJSM, liftJSM)

attachId ::
  MonadJSM  m =>
  T.Text ->
  (forall x. Widget x a) ->
  m (Maybe a)
attachId eid w =
  withJSContextSingleton $ \jsSing -> do
    doc <- currentDocumentUnchecked
    root <- getElementById doc eid
    case root of
      Nothing ->
        return Nothing
      Just docRoot -> do
        x <- liftJSM $ attachWidget docRoot jsSing w
        pure $ Just x

attachId_ ::
  MonadJSM  m =>
  T.Text ->
  (forall x. Widget x a) ->
  m ()
attachId_ eid w = do
  _ <- attachId eid w
  pure ()
