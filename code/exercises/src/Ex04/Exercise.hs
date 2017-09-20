{-# LANGUAGE CPP #-}
module Ex04.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex04.Common
import Ex04.Run

ex04 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex04 (Inputs bMoney bCarrot bCelery bCucumber bSelected eBuy eRefund) =
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
    host ex04
#endif
