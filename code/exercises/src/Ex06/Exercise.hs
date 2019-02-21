{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex06.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex06.Common
import Ex06.Run

ex06 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex06 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
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
  in do
    dChange <- holdDyn 0 $ leftmost [0 <$ eBuy, eChange]
    dVend   <- holdDyn "" $ leftmost ["" <$ eChange, errorText <$> eError, eVend]
    pure $ Outputs eVend eSpend eChange eError dChange dVend

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif
