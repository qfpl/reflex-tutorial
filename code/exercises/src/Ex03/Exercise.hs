{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex03.Exercise where

import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex03.Common
import Ex03.Run

ex03 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    product name | name == "Carrot" = carrot
                 | name == "Celery" = celery
                 | otherwise = cucumber
    eProduct = product <$> bSelected <@ eBuy
    eSale = difference eProduct eError
    eVend = pName <$> eSale
    eSpend = pCost <$> eSale
    eChange = bMoney <@ eRefund
    notEnough money cost = if cost > money then Just NotEnoughMoney else Nothing
    eError = attachWithMaybe notEnough bMoney (pCost <$> eProduct)
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex03
#endif
