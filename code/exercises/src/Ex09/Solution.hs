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

ex09 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
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

  -- We have move the display code out into functions, see below for the `*Row` functions.
  -- They are mostly about getting calls to the `dynText` and `button` functions into the right places

  eBuy    <- buyRow

  dMoney  <- dynMoney eAdd eSpend eRefund
  eAdd    <- moneyRow dMoney

  dChange <- dynChange eSpend eChange eError
  eRefund <- changeRow dChange

  dVend   <- dynVend eVend eSpend eError
  vendRow dVend

  pure eVend

buyRow ::
  MonadWidget t m =>
  m (Event t ())
buyRow =
  el "tr" $ do
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      button "Buy"

dynMoney ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t () ->
  Event t Money ->
  Event t () ->
  m (Dynamic t Money)
dynMoney eAdd eSpend eRefund = mdo
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

  pure dMoney

moneyRow ::
  ( MonadWidget t m
  ) =>
  Dynamic t Money ->
  m (Event t ())
moneyRow dMoney =
  el "tr" $ do
    el "td" $
      text "Money inserted:"
    el "td" $
      pure ()
    el "td" $
      dynText $ moneyDisplay <$> dMoney
    el "td" $
      button "Add money"

dynChange ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t Money ->
  Event t Money ->
  Event t Error ->
  m (Dynamic t Money)
dynChange eSpend eChange eError =
  holdDyn 0 .  leftmost $ [
      eChange
    , 0 <$ eSpend
    , 0 <$ eError
    ]

changeRow ::
  ( MonadWidget t m
  ) =>
  Dynamic t Money ->
  m (Event t ())
changeRow dChange =
  el "tr" $ do
    el "td" $
      text "Change:"
    el "td" $
      pure ()
    el "td" $
      dynText $ moneyDisplay <$> dChange
    el "td" $
      button "Refund"

dynVend ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t Text ->
  Event t Money ->
  Event t Error ->
  m (Dynamic t Text)
dynVend eVend eSpend eError =
  holdDyn "" .  leftmost $ [
     eVend
   , ""        <$  eSpend
   , errorText <$> eError
   ]

vendRow ::
  ( MonadWidget t m
  ) =>
  Dynamic t Text ->
  m ()
vendRow dVend =
  el "tr" $ do
    el "td" $
      text "Tray:"
    el "td" $
      pure ()
    el "td" $
      dynText dVend
    el "td" $
      pure ()

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
