{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex09.Solution (
    attachEx09
  ) where

import Language.Javascript.JSaddle (JSM)

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)

import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

import Util.Attach
import qualified Util.Bootstrap as B

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex09.Common
import Ex09.Run

ex09 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
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

  eBuy <-
    buyRow
  dMoney  <-
    moneyRow eSpend eRefund
  -- could compute eChange from eRefund internally if dMoney was passed in
  eRefund <-
    changeRow eSpend eChange eError
  vendRow eVend eSpend eError

  pure $ Outputs eVend

buyRow ::
  MonadWidget t m =>
  m (Event t ())
buyRow =
  row'
    (pure ())
    (pure ())
    (pure ())
    (B.button "Buy")

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
    -- track when we have at least one item in stock
    dNonZero = (0 <) <$> dQuantity
    -- only pass through vend events when we have stock
    eSub     = gate (current dNonZero) e

  -- We are only ever decreasing the stock by one unit
  dQuantity <- foldDyn ($) i $
    -- but only when the name matches and we have stock to vend
    subtract 1 <$ ffilter (== pName p) eSub
  pure $ Stock p <$> dQuantity

moneyRow ::
  ( MonadWidget t m
  ) =>
  Event t Money ->
  Event t () ->
  m (Dynamic t Money)
moneyRow eSpend eRefund = mdo
  let
    isOverspend money price =
      money < price
    eOverspend =
      isOverspend <$> current dMoney <@> eSpend
    eSpendOK =
      difference eSpend (ffilter id eOverspend)

  dMoney <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)    <$  eAdd
    , flip (-) <$> eSpendOK
    , const 0  <$  eRefund
    ]

  eAdd <- row'
    (text "Money inserted:")
    (pure ())
    (dynText $ moneyDisplay <$> dMoney)
    (B.button "Add money")

  pure dMoney

changeRow ::
  ( MonadWidget t m
  ) =>
  Event t Money ->
  Event t Money ->
  Event t Error ->
  m (Event t ())
changeRow eSpend eChange eError = do
  dChange <- holdDyn 0 .  leftmost $ [
      eChange
    , 0 <$ eSpend
    , 0 <$ eError
    ]
  row'
    (text "Change:")
    (pure ())
    (dynText $ moneyDisplay <$> dChange)
    (B.button "Refund")

vendRow ::
  ( MonadWidget t m
  ) =>
  Event t Text ->
  Event t Money ->
  Event t Error ->
  m ()
vendRow eVend eSpend eError = do
  dVend <- holdDyn "" .  leftmost $ [
     eVend
   , ""        <$  eSpend
   , errorText <$> eError
   ]
  row_
    (text "Tray:")
    (pure ())
    (pure ())
    (dynText dVend)

attachEx09 ::
  JSM ()
attachEx09 =
  attachId_ "ex09" $
    host mkStock ex09

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex09
#endif
