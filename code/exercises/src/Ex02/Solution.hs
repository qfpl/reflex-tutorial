{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex02.Solution (
    attachEx02
  ) where

import Language.Javascript.JSaddle (JSM)

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex02.Common
import Ex02.Run

ex02 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
  let
    eProduct =
      leftmost [
          carrot   <$ eCarrot
        , celery   <$ eCelery
        , cucumber <$ eCucumber
        ]

    checkNotEnoughMoney money p =
      money < pCost p
    eNotEnoughMoney =
      () <$ ffilter id (checkNotEnoughMoney <$> bMoney <@> eProduct)

    eSale =
      difference eProduct eNotEnoughMoney

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

attachEx02 ::
  JSM ()
attachEx02 =
  attachId_ "ex02" $
    host ex02

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex02
#endif
