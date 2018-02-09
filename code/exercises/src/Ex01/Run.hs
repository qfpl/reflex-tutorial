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

productWidget ::
  MonadWidget t m =>
  Product ->
  m (Event t ())
productWidget p =
  let
    r1 = text $ pName p
    r2 = text . moneyDisplay $ pCost p
    r3 = B.button "Buy"
  in
    row r1 r2 r3

host ::
  MonadWidget t m =>
  Ex01Fn t ->
  m ()
host fn = B.panel . grid $ mdo

  input <- do
      eCarrot <- productWidget carrot
      eCelery <- productWidget celery
      eCucumber <- productWidget cucumber
      pure $ Inputs eCarrot eCelery eCucumber eRefund

  dMoney <- trackMoney $ MoneyInputs eAdd eSpend eRefund
  eAdd   <- moneyRow dMoney

  let
    dOut =
      (\m -> fn m input) <$> dMoney
    eVend =
      switchDyn . fmap oeVend $ dOut
    eSpend =
      switchDyn . fmap oeSpend $ dOut
    eChange =
      switchDyn . fmap oeChange $ dOut
    eNotEnoughMoney =
      switchDyn . fmap oeNotEnoughMoney $ dOut
    outputs =
      Outputs eVend eSpend eChange eNotEnoughMoney

  dChange <- changeDisplay outputs
  eRefund <- changeRow dChange

  dVend <- vendDisplay outputs
  vendRow dVend

  pure ()

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
   , ""                   <$ eSpend
   , "Insufficient funds" <$ eError
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
