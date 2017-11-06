{-# LANGUAGE OverloadedStrings #-}
module Posts (
    posts
  ) where

import GHCJS.DOM.Types (JSM)

import Posts.Event (eventPostExamples)
import Posts.Behavior (behaviorPostExamples)
import Posts.Dynamic (dynamicPostExamples)
import Posts.RecursiveDo (recursiveDoPostExamples)
import Posts.Dom (domPostExamples)
import Posts.Switch (switchPostExamples)
import Posts.Component (componentPostExamples)
import Posts.Collection (collectionPostExamples)

import Util.Attach
import Util.Grid

posts :: JSM ()
posts = do
  attachId_ "grid-setup" $
    setupGrid defaultGridConfig
  eventPostExamples
  behaviorPostExamples
  dynamicPostExamples
  recursiveDoPostExamples
  domPostExamples
  switchPostExamples
  componentPostExamples
  collectionPostExamples
