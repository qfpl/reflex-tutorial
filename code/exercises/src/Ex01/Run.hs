{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex01.Run (
    host
  ) where

import Control.Monad.Fix (MonadFix)

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex01.Common

moneyDisplay ::
  Money ->
  Text
moneyDisplay =
  ("$" <>) . Text.pack . show

productWidget ::
  MonadWidget t m =>
  Product ->
  m (Event t ())
productWidget p = divClass "row" $ do
  divClass "col-md-3" $
    text $ pName p
  divClass "col-md-1" $
    text . moneyDisplay $ pCost p
  divClass "col-md-1" $
    B.button  "Buy"

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
  Ex01Fn t ->
  m ()
host fn = B.panel $ divClass "container" $ mdo

  input <- do
      eCarrot <- productWidget carrot
      eCelery <- productWidget celery
      eCucumber <- productWidget cucumber
      pure $ Inputs eCarrot eCelery eCucumber eRefund

  eAdd <- divClass "row" $ mdo
      divClass "col-md-3" $
        text "Money inserted:"
      divClass "col-md-1" $
        dynText $ moneyDisplay <$> dMoney
      divClass "col-md-1" $
        B.button "Add money"

  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund

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

  dChange <- holdDyn 0 . leftmost $ [
                 eChange
               , 0 <$ updated dMoney
               , 0 <$ eNotEnoughMoney
               ]

  eRefund <- divClass "row" $ do
    divClass "col-md-3" $
      text "Change:"
    divClass "col-md-1" $
      dynText $ moneyDisplay <$> dChange
    divClass "col-md-1" $
      B.button "Refund"

  dVend <- holdDyn "" . leftmost $ [
               eVend
             , "" <$ updated dMoney
             , "Insufficient funds" <$ eNotEnoughMoney
             ]

  divClass "row" $ do
    divClass "col-md-3" $
      text "Tray:"
    divClass "col-md-1" $
      dynText dVend
    divClass "col-md-1" $
      text ""

  pure ()

