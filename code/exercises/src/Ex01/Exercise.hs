{-# LANGUAGE CPP #-}
module Ex01.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex01.Common
import Ex01.Run

ex01 ::
  Reflex t =>
  Int ->
  Inputs t ->
  Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    eVend =
      never
    eSpend =
      never
    eChange =
      never
    eNotEnoughMoney =
      never
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $ 
    host ex01
#endif
