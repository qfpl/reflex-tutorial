{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex00.Exercise where

import Data.Text (Text)

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex00.Common
import Ex00.Run

ex00 ::
  Reflex t =>
  Event t () ->
  Event t () ->
  (Event t Text, Event t Text)
ex00 eFirst eSecond =
 ( "Better"        <$ eFirst
 , "Much Better"   <$ eSecond
 )

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $ 
    host ex00
#endif
