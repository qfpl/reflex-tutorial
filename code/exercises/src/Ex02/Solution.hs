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
    checkEnough money p =
      let
        cost = pCost p
      in
        if cost <= money 
        then Just p
        else Nothing
    eSale =
      attachWithMaybe checkEnough bMoney eProduct
    checkNotEnough money p =
      let
        cost = pCost p
      in
        if cost > money 
        then Just () 
        else Nothing
    eNotEnoughMoney =
      attachWithMaybe checkNotEnough bMoney eProduct
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
