{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex04.Run (
    host
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex04.Common

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
    dynText $ ("$" <>) . Text.pack . show . pCost . sProduct <$> dStock
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
  m (Dynamic t Int)
mkStock i p e =
  let
    n = pName p
  in
    foldDyn ($) i $
      subtract 1 <$ ffilter (== n) e

host ::
  MonadWidget t m =>
  Ex04Fn t ->
  m ()
host fn = B.panel $ divClass "container" $ mdo
  dCarrotStock <- mkStock 5 carrot eVend
  dCeleryStock <- mkStock 5 celery eVend
  dCucumberStock <- mkStock 5 cucumber eVend

  let
    dCarrot =
      Stock carrot <$> dCarrotStock
    dCelery =
      Stock celery <$> dCeleryStock
    dCucumber =
      Stock cucumber <$> dCucumberStock

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
          (current dMoney)
          (current dCarrot)
          (current dCelery)
          (current dCucumber)
          (current dSelected)
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

  dMoney <- divClass "row" $ mdo
      dMoney <- foldDyn ($) 0 . mergeWith (.) $ [
                  (+ 1)    <$ eAdd
                , flip (-) <$> eSpend
                , const 0  <$ eRefund
                ]
      divClass "col-md-3" $
        text "Money inserted:"
      divClass "col-md-1" $
        text ""
      divClass "col-md-1" $
        dynText $ ("$" <>) . Text.pack . show <$> dMoney
      eAdd <- divClass "col-md-1" $
        B.button "Add money"
      pure dMoney


  let
    Outputs eVend eSpend eChange eError = fn input
    eErrorText = errorText <$> eError

  eRefund <- divClass "row" $ do
    divClass "col-md-3" $
      text "Change:"
    divClass "col-md-1" $
      text ""
    divClass "col-md-1" $ do
      dChange <- holdDyn 0 .
                 leftmost $ [
                   eChange
                 , 0 <$ updated dMoney
                 , 0 <$ eError
                 ]
      dynText $ ("$" <>) . Text.pack . show <$> dChange
    divClass "col-md-1" $
      B.button "Refund"

  divClass "row" $ do
    divClass "col-md-3" $
      text "Tray:"
    divClass "col-md-1" $
      text ""
    divClass "col-md-1" $ do
      dVend <- holdDyn "" .
               leftmost $ [
                 eVend
               , "" <$ updated dMoney
               , eErrorText
               ]
      dynText dVend
    divClass "col-md-1" $
      text ""

  pure ()

