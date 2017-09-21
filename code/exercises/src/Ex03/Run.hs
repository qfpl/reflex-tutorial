{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex03.Run (
    host
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex03.Common

productWidget ::
  MonadWidget t m =>
  Dynamic t Product ->
  Dynamic t Text ->
  m (Event t Text)
productWidget dProduct dSelected = divClass "row" $ do
  divClass "col-md-3" $
    dynText $ pName <$> dProduct
  divClass "col-md-1" $
    dynText $ ("$" <>) . Text.pack . show . pCost <$> dProduct
  divClass "col-md-1" $ do
    let
      attrs =
        "type" =: "radio" <>
        "name" =: "product"
      mkAttrs p n =
        if pName p == n
        then "checked" =: ""
        else mempty
      dynAttrs = mkAttrs <$> dProduct <*> dSelected
    (e, _) <- elDynAttr' "input" (pure attrs <> dynAttrs) $ pure ()
    let eClick = domEvent Click e
    pure $ pName <$> current dProduct <@ eClick

host ::
  MonadWidget t m =>
  Ex03Fn t ->
  m ()
host fn = B.panel $ divClass "container" $ mdo

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

  eBuy <- divClass "row" $ do
      divClass "col-md-3" $
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

