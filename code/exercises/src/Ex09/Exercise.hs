{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex09.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex09.Common
import Ex09.Run

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

ex09 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  let
    eVend =
      never

  pure $ Outputs eVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex09
#endif
