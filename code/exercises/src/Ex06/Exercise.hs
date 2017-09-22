{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex06.Exercise where

import Control.Monad.Fix (MonadFix)

import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex06.Common
import Ex06.Run

ex06 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex06 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
  let
    eVend =
      never
    eSpend =
      never
    eChange =
      never
    eError =
      never
    dChange =
      pure 0
    dVend =
      pure ""
  in do
    pure $ Outputs eVend eSpend eChange eError dChange dVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif
