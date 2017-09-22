{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex08.Solution (
    attachEx08
  ) where

import Language.Javascript.JSaddle (JSM)

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)

import qualified Data.Map as Map

import Reflex

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex08.Common
import Ex08.Run

mkStock ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
mkStock i p e = mdo
  let
    dNonZero = (0 <) <$> dQuantity
    eSub     = gate (current dNonZero) e
  dQuantity <- foldDyn ($) i $
    subtract 1 <$ ffilter (== pName p) eSub
  pure $ Stock p <$> dQuantity

ex08 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex08 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
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

  dMoney <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)    <$  eAdd
    , flip (-) <$> eSpend
    , const 0  <$  eRefund
    ]

  dChange <- holdDyn 0 .  leftmost $ [
      eChange
    , 0 <$ eSpend
    , 0 <$ eError
    ]

  dVend <- holdDyn "" .  leftmost $ [
      eVend
    , ""        <$  eSpend
    , errorText <$> eError
    ]

  pure $ Outputs eVend eSpend eChange eError dMoney dChange dVend

attachEx08 ::
  JSM ()
attachEx08 =
  attachId_ "ex08" $
    host mkStock ex08

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex08
#endif
