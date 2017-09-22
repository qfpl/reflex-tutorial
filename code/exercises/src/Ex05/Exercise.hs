{-# LANGUAGE CPP #-}
module Ex05.Exercise where

import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex05.Common
import Ex05.Run

ex05 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex05 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
  let
    eVend =
      never
    eSpend =
      never
    eChange =
      never
    eError =
      never
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex05
#endif
