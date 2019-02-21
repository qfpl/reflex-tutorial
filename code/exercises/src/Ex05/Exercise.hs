{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
    toStock name | name == "Carrot" = dCarrot
                 | name == "Celery" = dCelery
                 | otherwise = dCucumber

    eStock = (current $ dSelected >>= toStock) <@ eBuy
    eSale = difference (sProduct <$> eStock) eError
    eVend = pName <$> eSale
    eSpend = pCost <$> eSale
    eChange = current dMoney <@ eRefund

    canBuy money (Stock (Product _ cost) qty)
      | qty < 1      = Just ItemOutOfStock
      | cost > money = Just NotEnoughMoney
      | otherwise    = Nothing

    eError = attachWithMaybe canBuy (current dMoney) eStock
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex05
#endif
