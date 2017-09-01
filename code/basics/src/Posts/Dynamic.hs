{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Posts.Dynamic (
    dynamicPostExamples
  ) where

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

dynamicPostExamples ::
  MonadJSM m =>
  m ()
dynamicPostExamples =
  attachId_ "basics-dynamic-counter"
    counterExample

counterExample ::
  MonadWidget t m =>
  m ()
counterExample = mdo
  el "div" $
    display dCount

  (eAdd, eClear) <- el "span" $
    (,) <$> button "Add" <*> button "Clear"

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1) <$ eAdd
    ]

  pure ()
