{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Dynamic (
    dynamicPostExamples
  ) where

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reflex

dynamicPostExamples ::
  MonadJSM m =>
  m ()
dynamicPostExamples = do
  attachId_ "basics-dynamic-counter-1"
    counterExample1
  attachId_ "basics-dynamic-counter-2"
    counterExample2

counterExample1 ::
  MonadWidget t m =>
  m ()
counterExample1 = divClass "panel panel-default" . divClass "panel-body" $ mdo
  el "div" $
    display dCount

  eAdd <- el "span" $
    buttonClass "btn btn-default" "Add"

  dCount <- foldDyn ($) 0 $
      (+ 1) <$ eAdd

  pure ()

counterExample2 ::
  MonadWidget t m =>
  m ()
counterExample2 = divClass "panel panel-default" . divClass "panel-body" $ mdo
  el "div" $
    display dCount

  (eAdd, eClear) <- el "span" $ do
    eAdd   <- buttonClass "btn btn-default" "Add"
    eClear <- buttonClass "btn btn-default" "Clear"
    pure (eAdd, eClear)

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1) <$ eAdd
    ]

  pure ()
