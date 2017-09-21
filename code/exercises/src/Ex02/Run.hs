{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Ex02.Run (
    host
  ) where

import Control.Monad.Fix (MonadFix)

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

import qualified Util.Bootstrap as B

import Ex02.Common

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
  Ex02Fn t ->
  m ()
host fn = B.panel $ divClass "container" $ mdo

  input <- do
    eCarrot <- productWidget carrot
    eCelery <- productWidget celery
    eCucumber <- productWidget cucumber
    pure $ Inputs (current dMoney) eCarrot eCelery eCucumber eRefund

  eAdd <- divClass "row" $ mdo
    divClass "col-md-3" $
      text "Money inserted:"
    divClass "col-md-1" $
      dynText $ moneyDisplay <$> dMoney
    divClass "col-md-1" $
      B.button "Add money"

  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund

  let
    outputs = fn input
    eSpend  = oeSpend outputs

  dChange <- changeDisplay outputs

  eRefund <- divClass "row" $ do
    divClass "col-md-3" $
      text "Change:"
    divClass "col-md-1" $
      dynText $ moneyDisplay <$> dChange
    divClass "col-md-1" $
      B.button "Refund"

  dVend <- vendDisplay outputs

  divClass "row" $ do
    divClass "col-md-3" $
      text "Tray:"
    divClass "col-md-1" $
      dynText dVend
    divClass "col-md-1" $
      text ""

  pure ()

