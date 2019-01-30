{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Ex03.Run (
    host
  ) where

import Control.Monad.Fix (MonadFix)

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex03.Common

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
  m c
row ma mb mc = elClass "div" "row" $
  (\_ _ x -> x)
    <$> elClass "div" "col-md-3" ma
    <*> elClass "div" "col-md-1" mb
    <*> elClass "div" "col-md-1" mc

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

productWidget ::
  MonadWidget t m =>
  Dynamic t Product ->
  Dynamic t Text ->
  m (Event t Text)
productWidget dProduct dSelected =
  let
    r1 = dynText $ pName <$> dProduct
    r2 = dynText $ moneyDisplay . pCost <$> dProduct
    r3 = radioButton "product" (pName <$> dProduct) dSelected
  in
    row r1 r2 r3

host ::
  MonadWidget t m =>
  Ex03Fn t ->
  m ()
host fn = B.panel . grid $ mdo

  input <- mdo
      eCarrot <-
        productWidget (pure carrot) dSelected
      eCelery <-
        productWidget (pure celery) dSelected
      eCucumber <-
        productWidget (pure cucumber) dSelected
      dSelected <-
        holdDyn (pName carrot) .
        leftmost $ [eCarrot, eCelery, eCucumber]
      pure $
        Inputs
          (current dMoney)
          (current dSelected)
          eBuy
          eRefund

  eBuy   <- buyRow
  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund
  eAdd   <- moneyRow dMoney

  let
    outputs = fn input
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
  row rBlank rBlank $
    B.button "Buy"

data MoneyInputs t =
  MoneyInputs
    (Event t ())    -- add
    (Event t Money) -- spend
    (Event t ())    -- refund

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
    r2 = dynText $ moneyDisplay <$> dMoney
    r3 = B.button "Add money"
  in
    row r1 r2 r3

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
    r2 = dynText $ moneyDisplay <$> dChange
    r3 = B.button "Refund"
  in
    row r1 r2 r3

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
    r2     = dynText dVend
    rBlank = pure ()
  in
    row r1 r2 rBlank
