{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex10.Solution (
    attachEx10
  ) where

import Language.Javascript.JSaddle (JSM)

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)

import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

import Util.Attach

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex10.Common
import Ex10.Run

grid ::
  MonadWidget t m =>
  m a ->
  m a
grid =
  elClass "div" "container"

row ::
  MonadWidget t m =>
  m a ->
  m b ->
  m c ->
  m d ->
  m d
row ma mb mc md = elClass "div" "row" $
  (\_ _ _ x -> x)
    <$> elClass "div" "col-md-3" ma
    <*> elClass "div" "col-md-1" mb
    <*> elClass "div" "col-md-1" mc
    <*> elClass "div" "col-md-1" md

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

ex10 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex10 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
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
  let
    rBlank = pure ()
  in
  row rBlank rBlank rBlank $
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
  let
    r1 = text "Money inserted:"
    r2 = pure ()
    r3 = dynText $ moneyDisplay <$> dMoney
    r4 = button "Add money"
  in
    row r1 r2 r3 r4

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
  let
    r1 = text "Change:"
    r2 = pure ()
    r3 = dynText $ moneyDisplay <$> dChange
    r4 = button "Refund"
  in
    row r1 r2 r3 r4

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
  let
    r1     = text "Tray:"
    rBlank = pure ()
    r3     = dynText dVend
  in
    row r1 rBlank r3 rBlank

attachEx10 ::
  JSM ()
attachEx10 =
  attachId_ "ex10" $
    host grid row mkStock ex10

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host grid row mkStock ex10
#endif
