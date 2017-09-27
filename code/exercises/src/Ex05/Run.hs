{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex05.Run (
    host
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex05.Common

moneyDisplay ::
  Money ->
  Text
moneyDisplay =
  ("$" <>) . Text.pack . show

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

radioButton ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Text ->
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioButton name dValue dSelected =
  let
    attrs =
      "type" =: "radio" <>
      "name" =: name
    mkAttrs a n =
      if a == n
      then "checked" =: ""
      else mempty
    dynAttrs = mkAttrs <$> dValue <*> dSelected
  in do
    (e, _) <- elDynAttr' "input" (pure attrs <> dynAttrs) $ pure ()
    let eClick = domEvent Click e
    pure $ current dValue <@ eClick

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected =
  let
    r1 = dynText $ pName . sProduct <$> dStock
    r2 = dynText $ Text.pack . show . sQuantity <$> dStock
    r3 = dynText $ moneyDisplay . pCost . sProduct <$> dStock
    r4 = radioButton "stock" ((pName . sProduct) <$> dStock) dSelected
  in
    row r1 r2 r3 r4

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

host ::
  MonadWidget t m =>
  Ex05Fn t ->
  m ()
host fn = B.panel . grid $ mdo
  dCarrot   <- mkStock 5 carrot   eVend
  dCelery   <- mkStock 5 celery   eVend
  dCucumber <- mkStock 5 cucumber eVend

  input <- mdo
      eCarrot <-
        stockWidget dCarrot dSelected
      eCelery <-
        stockWidget dCelery dSelected
      eCucumber <-
        stockWidget dCucumber dSelected
      dSelected <-
        holdDyn (pName carrot) .
        leftmost $ [eCarrot, eCelery, eCucumber]
      pure $
        Inputs
          dMoney
          dCarrot
          dCelery
          dCucumber
          dSelected
          eBuy
          eRefund

  eBuy   <- buyRow
  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund
  eAdd   <- moneyRow dMoney

  let
    outputs = fn input
    eVend   = oeVend outputs
    eSpend  = oeSpend outputs

  dChange <- changeDisplay outputs
  eRefund <- changeRow dChange

  dVend <- vendDisplay outputs
  vendRow dVend

  pure ()

buyRow ::
  MonadWidget t m =>
  m (Event t ())
buyRow =
  let
    rBlank = pure ()
  in
  row rBlank rBlank rBlank $
    B.button "Buy"

data MoneyInputs t =
  MoneyInputs {
    mieAdd    :: Event t ()
  , mieSpend  :: Event t Money
  , mieRefund :: Event t ()
  }

trackMoney ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  MoneyInputs t ->
  m (Dynamic t Money)
trackMoney (MoneyInputs eAdd eSpend eRefund) =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)    <$  eAdd
    , flip (-) <$> eSpend
    , const 0  <$  eRefund
    ]

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
    r4 = B.button "Add money"
  in
    row r1 r2 r3 r4

changeDisplay ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Outputs t ->
  m (Dynamic t Money)
changeDisplay (Outputs _ eSpend eChange eError) =
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
    r4 = B.button "Refund"
  in
    row r1 r2 r3 r4

vendDisplay ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Outputs t ->
  m (Dynamic t Text)
vendDisplay (Outputs eVend eSpend _ eError) =
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
