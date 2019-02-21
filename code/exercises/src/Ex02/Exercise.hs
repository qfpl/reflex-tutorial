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
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
  let
    eProduct = leftmost [carrot <$ eCarrot, celery <$ eCelery, cucumber <$ eCucumber]
    eSale = difference eProduct eError
    eVend = pName <$> eSale
    eSpend = pCost <$> eSale
    eChange = tag bMoney eRefund
    notEnough money cost = if cost > money then Just NotEnoughMoney else Nothing
    eError = attachWithMaybe notEnough bMoney (pCost <$> eProduct)
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex02
#endif
