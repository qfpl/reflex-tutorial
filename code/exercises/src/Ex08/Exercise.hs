{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex08.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex08.Common
import Ex08.Run

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

ex08 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex08 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
  let
    eVend =
      never
    eSpend =
      never
    eChange =
      never
    eError =
      never
    dMoney =
      pure 0
    dChange =
      pure 0
    dVend =
      pure ""

  pure $ Outputs eVend eSpend eChange eError dMoney dChange dVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex08
#endif
