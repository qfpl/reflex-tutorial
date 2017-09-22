{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex05.Solution (
    attachEx05
  ) where

import Language.Javascript.JSaddle (JSM)

import qualified Data.Map as Map

import Reflex

import Util.Attach

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
    dStocks =
      [dCarrot, dCelery, dCucumber]
    stockSingleton s =
      Map.singleton (pName . sProduct $ s) s
    dmStock =
      foldMap (fmap stockSingleton) dStocks
    emStock =
      Map.lookup <$> current dSelected <*> current dmStock <@ eBuy
    eStock =
      fmapMaybe id emStock

    checkItemOutOfStock s =
      sQuantity s == 0
    eItemOutOfStock =
      ItemOutOfStock <$ ffilter checkItemOutOfStock eStock

    checkNotEnoughMoney money s =
      money < (pCost . sProduct $ s)
    eNotEnoughMoney =
      NotEnoughMoney <$ ffilter id (checkNotEnoughMoney <$> current dMoney <@> eStock)

    eError =
      leftmost [
        eItemOutOfStock
      , eNotEnoughMoney
      ]

    eSale =
      sProduct <$> difference eStock eError

    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale

    eChange =
      current dMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

attachEx05 ::
  JSM ()
attachEx05 =
  attachId_ "ex05" $
    host ex05

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex05
#endif
