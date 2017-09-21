{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex01.Solution (
    attachEx01
  ) where

import Language.Javascript.JSaddle (JSM)

import Reflex

import Util.Attach
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
    eProduct =
      leftmost [
          carrot   <$ eCarrot
        , celery   <$ eCelery
        , cucumber <$ eCucumber
        ]

    eNotEnoughMoney =
      () <$ ffilter ((money <) . pCost) eProduct

    eSale =
      difference eProduct eNotEnoughMoney

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      money <$ eRefund
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

attachEx01 ::
  JSM ()
attachEx01 =
  attachId_ "ex01" $
    host ex01

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif
