{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex07.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex07.Common
import Ex07.Run

ex07 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex07 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
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
    host ex07
#endif
