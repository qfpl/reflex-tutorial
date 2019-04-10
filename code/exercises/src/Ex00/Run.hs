{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Ex00.Run (
    host
  ) where

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex00.Common

host ::
  MonadWidget t m =>
  Ex00Fn t ->
  m ()
host fn = B.panel . divClass "card my-2" . divClass "card-body" . divClass "container" $ mdo

  eFirst <- divClass "row" $  do
    eFirst' <- divClass "col-md-3" $
      B.button "First"
    divClass "col-md-3 ex00" $
      dynText dFirst
    pure eFirst'

  eSecond <- divClass "row" $  do
    eSecond' <- divClass "col-md-3" $
      B.button "Second"
    divClass "col-md-3 ex00" $
      dynText dSecond
    pure eSecond'

  let
    (eFirstOut, eSecondOut) =
      fn eFirst eSecond

  dFirst <- holdDyn "" .
           leftmost $ [eFirstOut, "" <$ eSecondOut]
  dSecond <- holdDyn "" .
           leftmost $ [eSecondOut, "" <$ eFirstOut]

  pure ()

