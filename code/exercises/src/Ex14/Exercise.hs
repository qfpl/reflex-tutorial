{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex14.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex14.Common
import Ex14.Run

ex14 ::
  ( MonadWidget t m
  ) =>
  m ()
ex14 = do
  error "TODO"

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex14
#endif
