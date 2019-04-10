{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Ex14.Run (
    host
  ) where

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

host ::
  MonadWidget t m =>
  m () ->
  m ()
host = B.panel . divClass "card my-2" . divClass "card-body"

