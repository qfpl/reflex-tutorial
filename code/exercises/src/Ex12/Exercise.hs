{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex12.Exercise where

import Control.Monad.Fix (MonadFix)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex12.Common
import Ex12.Run

radioCheckbox ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioCheckbox dValue dSelected =
  pure never

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected =
  pure never

grid ::
  MonadWidget t m =>
  m a ->
  m a
grid =
  error "TODO"

mkStock ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
mkStock =
  error "TODO"

ex12 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex12 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  let
    eVend =
      never

  pure eVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host grid stockWidget mkStock ex12
#endif
