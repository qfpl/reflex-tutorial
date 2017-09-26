{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex09.Run (
    host
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex09.Common

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

    attrs =
      "type" =: "radio" <>
      "name" =: "stock"
    mkAttrs s n =
      if (pName . sProduct) s == n
      then "checked" =: ""
      else mempty
    dynAttrs = mkAttrs <$> dStock <*> dSelected

    r4 = do
      (e, _) <- elDynAttr' "input" (pure attrs <> dynAttrs) $ pure ()
      let eClick = domEvent Click e
      pure $ pName . sProduct <$> current dStock <@ eClick
  in
    row' r1 r2 r3 r4

host ::
  MonadWidget t m =>
  Ex09FnA t m ->
  Ex09FnB t m ->
  m ()
host mkStock fn = B.panel . table $ mdo
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
        leftmost $ [
            eCarrot
          , eCelery
          , eCucumber
          ]
      pure $
        Inputs
          dCarrot
          dCelery
          dCucumber
          dSelected

  outputs <- fn input
  let
    eVend   = oeVend outputs


  pure ()

