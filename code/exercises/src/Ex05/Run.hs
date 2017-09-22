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

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected = divClass "row" $ do
  divClass "col-md-3" $
    dynText $ pName . sProduct <$> dStock
  divClass "col-md-1" $
    dynText $ Text.pack . show . sQuantity <$> dStock
  divClass "col-md-1" $
    dynText $ moneyDisplay . pCost . sProduct <$> dStock
  divClass "col-md-1" $ do
    let
      attrs =
        "type" =: "radio" <>
        "name" =: "stock"
      mkAttrs s n =
        if (pName . sProduct) s == n
        then "checked" =: ""
        else mempty
      dynAttrs = mkAttrs <$> dStock <*> dSelected
    (e, _) <- elDynAttr' "input" (pure attrs <> dynAttrs) $ pure ()
    let eClick = domEvent Click e
    pure $ pName . sProduct <$> current dStock <@ eClick

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

host ::
  MonadWidget t m =>
  Ex05Fn t ->
  m ()
host fn = B.panel $ divClass "container" $ mdo
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

  eBuy <- divClass "row" $ do
      divClass "col-md-3" $
        text ""
      divClass "col-md-1" $
        text ""
      divClass "col-md-1" $
        text ""
      divClass "col-md-1" $
        B.button "Buy"

  eAdd <- divClass "row" $ do
      divClass "col-md-3" $
        text "Money inserted:"
      divClass "col-md-1" $
        text ""
      divClass "col-md-1" $
        dynText $ moneyDisplay <$> dMoney
      divClass "col-md-1" $
        B.button "Add money"

  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund

  let
    outputs = fn input
    eVend   = oeVend outputs
    eSpend  = oeSpend outputs

  dChange <- changeDisplay outputs

  eRefund <- divClass "row" $ do
    divClass "col-md-3" $
      text "Change:"
    divClass "col-md-1" $
      text ""
    divClass "col-md-1" $
      dynText $ moneyDisplay <$> dChange
    divClass "col-md-1" $
      B.button "Refund"

  dVend <- vendDisplay outputs

  divClass "row" $ do
    divClass "col-md-3" $
      text "Tray:"
    divClass "col-md-1" $
      text ""
    divClass "col-md-1" $
      dynText dVend
    divClass "col-md-1" $
      text ""

  pure ()

