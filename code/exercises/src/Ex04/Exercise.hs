{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex04.Exercise where

import qualified Data.Map as Map

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
    toStock name | name == "Carrot" = bCarrot
                 | name == "Celery" = bCelery
                 | otherwise = bCucumber

    eStock = (bSelected >>= toStock) <@ eBuy
    eSale = difference (sProduct <$> eStock) eError
    eVend = pName <$> eSale
    eSpend = pCost <$> eSale
    eChange = bMoney <@ eRefund

    canBuy money (Stock (Product _ cost) qty)
      | qty < 1      = Just ItemOutOfStock
      | cost > money = Just NotEnoughMoney
      | otherwise    = Nothing

    eError = attachWithMaybe canBuy bMoney eStock
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex04
#endif
