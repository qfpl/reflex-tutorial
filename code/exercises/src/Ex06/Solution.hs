{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex06.Solution (
    attachEx06
  ) where

import Language.Javascript.JSaddle (JSM)

import Control.Monad.Fix (MonadFix)

import qualified Data.Map as Map

import Reflex

import Util.Attach

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
  in do
    dChange <- holdDyn 0 .  leftmost $ [
        eChange
      , 0 <$ eSpend
      , 0 <$ eError
      ]

    dVend <-holdDyn "" .  leftmost $ [
        eVend
      , ""        <$  eSpend
      , errorText <$> eError
      ]

    pure $ Outputs eVend eSpend eChange eError dChange dVend

attachEx06 ::
  JSM ()
attachEx06 =
  attachId_ "ex06" $
    host ex06

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif
