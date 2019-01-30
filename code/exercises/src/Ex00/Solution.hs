{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex00.Solution (
    attachEx00
  ) where

import Language.Javascript.JSaddle (JSM)

import Data.Text (Text)

import Reflex

import Util.Attach
#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex00.Run

ex00 ::
  Reflex t =>
  Event t () ->
  Event t () ->
  (Event t Text, Event t Text)
ex00 eFirst eSecond =
  ( "Better"      <$ eFirst
  , "Much better" <$ eSecond
  )

attachEx00 ::
  JSM ()
attachEx00 =
  attachId_ "ex00" $
    host ex00

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex00
#endif
