{-# LANGUAGE CPP #-}
module Ex02.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex02.Common
import Ex02.Run

ex02 ::
  Reflex t =>
  Int ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
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
    host ex02
#endif
