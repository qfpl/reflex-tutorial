{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex01.Run (
    host
  ) where

import Data.Monoid ((<>))

import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex01.Common

productWidget ::
  MonadWidget t m =>
  Product ->
  m (Event t ())
productWidget p = divClass "row" $ do
  divClass "col-md-3" $
    text $ pName p
  divClass "col-md-1" $
    text . ("$" <>) . Text.pack . show $ pCost p
  divClass "col-md-1" $
    B.button  "Buy"

host ::
  MonadWidget t m =>
  Ex01Fn t ->
  m ()
host fn = divClass "container" $ mdo

  input <- do
      eCarrot <- productWidget carrot
      eCelery <- productWidget celery
      eCucumber <- productWidget cucumber
      pure $ Inputs eCarrot eCelery eCucumber eRefund

  dMoney <- divClass "row" $ mdo
      dMoney <- foldDyn ($) 0 . mergeWith (.) $ [
                  (+ 1)    <$ eAdd
                , flip (-) <$> eSpend
                , const 0  <$ eRefund
                ]
      divClass "col-md-3" $
        text "Supplied:"
      divClass "col-md-1" $
        dynText $ ("$" <>) . Text.pack . show <$> dMoney
      eAdd <- divClass "col-md-1" $
        B.button "Add money"
      pure dMoney

  let
    dOut =
      (\m -> fn m input) <$> dMoney
    eVend =
      switch . current . fmap oeVend $ dOut
    eSpend =
      switch . current . fmap oeSpend $ dOut
    eChange =
      switch . current . fmap oeChange $ dOut
    eNotEnoughMoney =
      switch . current . fmap oeNotEnoughMoney $ dOut

  eRefund <- divClass "row" $ do
    divClass "col-md-3" $
      text "Change:"
    divClass "col-md-1" $ do
      dChange <- holdDyn 0 . leftmost $ [eChange, 0 <$ updated dMoney, 0 <$ eNotEnoughMoney]
      dynText $ ("$" <>) . Text.pack . show <$> dChange
    divClass "col-md-1" $
      B.button "Refund"

  divClass "row" $ do
    divClass "col-md-3" $
      text "Tray:"
    divClass "col-md-1" $ do
      dVend <- holdDyn "" . leftmost $ [eVend, "" <$ updated dMoney, "Insufficient funds" <$ eNotEnoughMoney]
      dynText dVend
    divClass "col-md-1" $
      text ""

  pure ()

